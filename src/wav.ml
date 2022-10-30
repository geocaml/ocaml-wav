type src = Cstruct.t -> int
type dst = (Cstruct.t * int * int) option -> unit

type header = {
  size : int32;
  fmt_size : int32;
  audio_format : [ `PCM | `Other of int ];
  num_channels : int;
  sample_rate : int32;
  byte_rate : int32;
  block_align : int;
  bits_per_sample : int;
  data_size : int32;
}

let sample_size header = header.bits_per_sample / 8
let sample_rate h = h.sample_rate

let pp_format ppf = function
  | `PCM -> Fmt.pf ppf "PCM"
  | `Other n -> Fmt.pf ppf "%i" n

let pp_header ppf (h : header) =
  Fmt.pf ppf "%a"
    (Fmt.record
       [
         (fun ppf h -> Fmt.pf ppf "size: %a" Fmt.int32 h.size);
         (fun ppf h -> Fmt.pf ppf "fmt_size: %a" Fmt.int32 h.fmt_size);
         (fun ppf h -> Fmt.pf ppf "audio_format: %a" pp_format h.audio_format);
         (fun ppf h -> Fmt.pf ppf "num_channels: %a" Fmt.int h.num_channels);
         (fun ppf h -> Fmt.pf ppf "sample_rate: %a" Fmt.int32 h.sample_rate);
         (fun ppf h -> Fmt.pf ppf "byte_rate: %a" Fmt.int32 h.byte_rate);
         (fun ppf h -> Fmt.pf ppf "block_align: %a" Fmt.int h.block_align);
         (fun ppf h ->
           Fmt.pf ppf "bits_per_sample: %a" Fmt.int h.bits_per_sample);
         (fun ppf h -> Fmt.pf ppf "data_size: %a" Fmt.int32 h.data_size);
       ])
    h

(* A reader is largely based off of Eio.Buf_read *)
type reader = {
  header : header;
  mutable src : src option;
  mutable buf : Cstruct.buffer;
  mutable pos : int;
  mutable len : int;
  mutable consumed : int; (* Total bytes consumed so far *)
  max_size : int;
}

let header r = r.header

let reader ?initial_size ?(max_size = max_int) src =
  let initial_size = Option.value initial_size ~default:(min 4096 max_size) in
  let buf = Cstruct.create 44 in
  let _ = src buf in
  let riff = Cstruct.to_string ~off:0 ~len:4 buf in
  assert (String.equal riff "RIFF");
  let file_size = Cstruct.LE.get_uint32 buf 4 in
  let fmt_size = Cstruct.LE.get_uint32 buf 16 in
  let audio_format =
    Cstruct.LE.get_uint16 buf 20 |> function 1 -> `PCM | n -> `Other n
  in
  let num_channels = Cstruct.LE.get_uint16 buf 22 in
  let sample_rate = Cstruct.LE.get_uint32 buf 24 in
  let byte_rate = Cstruct.LE.get_uint32 buf 28 in
  let block_align = Cstruct.LE.get_uint16 buf 32 in
  let bits_per_sample = Cstruct.LE.get_uint16 buf 34 in
  let data_size = Cstruct.LE.get_uint32 buf 40 in
  let header =
    {
      size = file_size;
      fmt_size;
      audio_format;
      num_channels;
      sample_rate;
      byte_rate;
      block_align;
      bits_per_sample;
      data_size;
    }
  in
  let wave = Cstruct.to_string ~off:8 ~len:4 buf in
  assert (String.equal wave "WAVE");
  {
    src = Some src;
    header;
    pos = 0;
    len = 0;
    consumed = 0;
    buf = Bigarray.(Array1.create char c_layout initial_size);
    max_size;
  }

let consume_err t n =
  Fmt.invalid_arg "Can't consume %d bytes of a %d byte buffer!" n t.len

let peek t = Cstruct.of_bigarray ~off:t.pos ~len:t.len t.buf

let[@inline] consume t n =
  if n < 0 || n > t.len then consume_err t n;
  t.pos <- t.pos + n;
  t.len <- t.len - n;
  t.consumed <- t.consumed + n

let capacity t = Bigarray.Array1.dim t.buf

let ensure_slow_path t n =
  assert (n >= 0);
  if n > t.max_size then raise (Failure "Buffer Limit Exceeded");
  (* We don't have enough data yet, so we'll need to do a read. *)
  match t.src with
  | None -> raise End_of_file
  | Some src -> (
      (* If the buffer is empty, we might as well use all of it: *)
      if t.len = 0 then t.pos <- 0;
      let () =
        let cap = capacity t in
        if n > cap then (
          (* [n] bytes won't fit. We need to resize the buffer. *)
          let new_size = max n (min t.max_size (cap * 2)) in
          let new_buf = Bigarray.(Array1.create char c_layout new_size) in
          Cstruct.blit (peek t) 0 (Cstruct.of_bigarray new_buf) 0 t.len;
          t.pos <- 0;
          t.buf <- new_buf)
        else if t.pos + n > cap then (
          (* [n] bytes will fit in the existing buffer, but we need to compact it first. *)
          Cstruct.blit (peek t) 0 (Cstruct.of_bigarray t.buf) 0 t.len;
          t.pos <- 0)
      in
      try
        while t.len < n do
          let free_space = Cstruct.of_bigarray t.buf ~off:(t.pos + t.len) in
          assert (t.len + Cstruct.length free_space >= n);
          let got = src free_space in
          t.len <- t.len + got
        done;
        assert (t.len >= n)
      with End_of_file ->
        t.src <- None;
        raise End_of_file)

let ensure t n = if t.len < n then ensure_slow_path t n

let is_int8 r =
  match (r.header.audio_format, r.header.bits_per_sample) with
  | `PCM, 8 -> true
  | _ -> false

let is_int16 r =
  match (r.header.audio_format, r.header.bits_per_sample) with
  | `PCM, 16 -> true
  | _ -> false

let is_float32 r =
  match (r.header.audio_format, r.header.bits_per_sample) with
  | _, 32 -> true
  | _ -> false

let raw_sample_exn reader =
  try
    let sample_size = reader.header.bits_per_sample / 8 in
    ensure reader sample_size;
    let c = Cstruct.sub (peek reader) 0 sample_size in
    consume reader sample_size;
    c
  with End_of_file -> raise Not_found

let raw_sample e = try Some (raw_sample_exn e) with Not_found -> None

let raw_samples reader =
  ensure reader (Int32.to_int reader.header.data_size);
  Cstruct.to_bigarray (peek reader)

let sample_int16_exn r =
  let buf = raw_sample_exn r in
  Cstruct.LE.get_uint16 buf 0

let sample_int16 r = try Some (sample_int16_exn r) with _ -> None

(* open Ctypes *)

let check_type t r =
  if t r then ()
  else invalid_arg "Trying to convert to an incompatible bigarray!"

let samples_int16 r =
  check_type is_int16 r;
  let sample_size = r.header.bits_per_sample / 8 in
  let samples = Int32.to_int r.header.data_size / sample_size / r.header.num_channels in
  let ba = Bigarray.Array1.(create Int16_signed C_layout samples) in
  for i = 0 to samples - 1 do
    Bigarray.Array1.set ba i (sample_int16_exn r)
  done;
  ba


module Signal_ext = struct
  include Owl_signal
  module N = Owl_dense_ndarray_generic

  let pad_or_sub nd n =
    let l = Owl_dense_ndarray_z.nth_dim nd 0 in
    if n < l then Owl_dense_ndarray_z.slice_left nd [| n |] else
    if n > l then Owl_dense_ndarray_z.pad ~v:Complex.zero [ [ 0; n - l ] ] nd
    else nd 
    

  let hilbert axis nd =
    let n = (N.shape nd).(axis) in
    let xf = Owl_fft_d.rfft ~axis nd in
    let xf = pad_or_sub xf n in
    let h = N.zeros Bigarray.complex64 [| n |] in
    if n mod 2 = 0 then begin
      N.set h [| 0 |] Complex.one; 
      N.set h [| n / 2 |] Complex.one;
      for i = 1 to n / 2 do
        N.set h [| i |] Complex.(add one one)
      done
    end else begin
      N.set h [| 0 |] Complex.one; 
      for i = 1 to (n + 1) / 2 do
        N.set h [| i |] Complex.(add one one)
      done
    end;
    Printf.printf "N: %i %i %i\n%!" n (Owl_dense_ndarray_z.nth_dim xf 0) (Owl_dense_ndarray_z.nth_dim h 0);
    Owl_dense_matrix_z.(mul_ xf h);
    Owl_fft_d.irfft ~axis xf
end