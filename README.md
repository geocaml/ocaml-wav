ocaml-wav
---------

WAV file reader and writer in pure OCaml.

## Reading WAV Files

Reading a WAV file requires constructing a WAV reader. This requires a function for filling a buffer with more bytes, i.e. a source.

```ocaml
# Wav.reader;;
- : ?initial_size:int -> ?max_size:int -> Wav.src -> Wav.reader = <fun>
```

The best way to do this is with Eio, the API was designed with that in mind. Eio's `Flow.source` is perfect for a `Wav.src`. Here we construct a function for building a `with_source` function which will automatically close the opened file for us thanks to `with_open_in`.

```ocaml
# let src ~flow buf = Eio.Flow.read flow buf;;
val src : flow:#Eio.Flow.source -> Cstruct.t -> int = <fun>
# let with_reader ~fs fn =
    Eio.Path.(with_open_in (fs / "data" / "noaa-example.wav")) @@ fun flow ->
    fn @@ Wav.reader (src ~flow);;
val with_reader : fs:#Eio.Fs.dir Eio.Path.t -> (Wav.reader -> 'a) -> 'a =
  <fun>
```

We can then use it to read the header of the WAV file for example.

```ocaml
# Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.cwd env in
  with_reader ~fs @@ fun r ->
  Fmt.pr "%a%!" Wav.pp_header (Wav.header r);;
size: 19840738
fmt_size: 16
audio_format: PCM
num_channels: 1
sample_rate: 11025
byte_rate: 22050
block_align: 2
bits_per_sample: 16
data_size: 19840702
- : unit = ()
```

Reader's can also produce the next sample if available using the `Wav.sample` function. This will move along every time it is called until reaching the end of the WAV file in which case it will return `None`. `Wav.sample_exn` is the same but raises `Not_found` instead of wrapping the sample as an `option` type.

```ocaml
# Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.cwd env in
  with_reader ~fs @@ fun r ->
  let rec loop acc =
    match Wav.sample_exn r with
    | s -> loop (s :: acc)
    | exception Not_found -> List.rev acc
  in
  let samples = loop [] in
  Fmt.pr "Number of samples: %i\nFirst sample: %a%!"
    (List.length samples) Cstruct.hexdump_pp (List.hd samples);;
Number of samples: 9920607
First sample: d9 21
- : unit = ()
```

### Motivating Example: NOAA Satellite Imagery

The initial motivation for building this OCaml library was to start constructing tools for analysing NOAA weather images which are transmitted in WAV format. In particular following [An accessible browser-based decoder for NOAA images](https://publiclab.org/notes/sashae/05-03-2022/an-accessible-browser-based-decoder-for-noaa-images). Getting from the raw WAV file to an image should be good example of using the library.

The first part of the process is to have a look at our WAV file:

```ocaml
# Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.cwd env in
  with_reader ~fs @@ fun r ->
  Fmt.pr "%a%!" Wav.pp_header (Wav.header r);;
size: 19840738
fmt_size: 16
audio_format: PCM
num_channels: 1
sample_rate: 11025
byte_rate: 22050
block_align: 2
bits_per_sample: 16
data_size: 19840702
- : unit = ()
```

We can see some important information, such as:

 - There is only one channel.
 - There are `11025` samples per second.

The Automatic Picture Transmission (APT) format tells us that the 4160 words per second which means we'll first have to downsample the data.

```ocaml
# Wav_conv.float32;;
- : Wav.reader ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
= <fun>
# Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.cwd env in
  with_reader ~fs @@ fun r -> 
  let ba = Wav_conv.float32 r in
  Float.abs @@ Bigarray.Array1.get ba 34000;;
```
