open Owl_plplot
open Owl

let src ~flow buf = Eio.Flow.read flow buf

let with_reader ~fs fn =
  Eio.Path.(with_open_in (fs / "data" / "noaa-example.wav")) @@ fun flow ->
  fn @@ Wav.reader (src ~flow)

let plot length ys =
  let h = Plot.create "plot.png" in
  let xs = Mat.linspace 0. (float_of_int length) length in
  Plot.plot ~h ~spec:[ RGB (255,0,0); LineStyle 1 ] xs ys;
  Plot.output h

let chunk_writer_of_flow flow = function
  | `String s -> Ok (Eio.Flow.copy_string s flow)
  | `Close -> Ok (Eio.Flow.close flow)

let image path r nd =
  let width = (Wav.header r |> Wav.sample_rate |> Int32.to_int) / 2 in
  let height = Bigarray.Genarray.nth_dim nd 0 / width in
  let img = Image.create_grey ~max_val:255 width height in
  for j = 0 to height - 1 do
    for i = 0 to width - 1 do
      let f = Bigarray.Genarray.get nd [| j * width + i |] in
      Image.write_grey img i j (int_of_float f / 70 |> fun x -> if x > 255 then 255 else x)
    done
  done;
  Eio.Path.with_open_out ~create:(`If_missing 0o666) path @@ fun flow ->
  let cw = chunk_writer_of_flow flow in
  ImagePNG.write cw img 

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.cwd env in
  let path = Eio.Path.(fs / "noaa.png") in
  with_reader ~fs @@ fun r ->
  Format.printf "%a%!" Wav.pp_header (Wav.header r);
  let ba = Wav.samples_int16 r in
  let ba = Bigarray.genarray_of_array1 @@ Wav_conv.int16_to_float64 ba in 
  let ba = Wav.Signal_ext.hilbert 0 ba in
  let ba = Owl_dense_ndarray_d.abs ba in
  image path r ba
  (* plot 1000 (Bigarray.Genarray.sub_left ba 34000 35000) *)

