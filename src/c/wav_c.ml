open Ctypes

(* Conversion functions for common data kinds when doing signal
   processing, particularly from the standard Cstruct byte bigarray. *)
let float32 r =
  let bytes = Wav.(sample_size @@ header r) in
  let samples = Wav.raw_samples r in
  let length = Bigarray.Array1.dim samples in
  let start = coerce (ptr char) (ptr double) (bigarray_start array1 samples) in
  Bigarray.(bigarray_of_ptr array1 length Float32 start)
