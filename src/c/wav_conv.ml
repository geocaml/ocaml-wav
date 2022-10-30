open Ctypes


(* Conversion functions for common data kinds when doing signal
   processing, particularly from the standard Cstruct byte bigarray. *)
let float32 r =
  let samples = Wav.raw_samples r in
  let length = Bigarray.Array1.dim samples in
  let start = coerce (ptr char) (ptr float) (bigarray_start array1 samples) in
  Bigarray.(bigarray_of_ptr array1 length Float32 start)



let int16_to_float64 ba =
   let length = Bigarray.Array1.dim ba in
   let new_arr = Bigarray.Array1.(create Float64 C_layout length) in 
   for i = 0 to length - 1 do
      let v = Bigarray.Array1.get ba i in
      Bigarray.Array1.set new_arr i (float_of_int v)
   done;
   new_arr
