include Owl_signal
module N = Owl_dense_ndarray_generic

let pad_or_sub nd n =
  let l = Owl_dense_ndarray_z.nth_dim nd 0 in
  if n < l then Owl_dense_ndarray_z.slice_left nd [| n |]
  else if n > l then Owl_dense_ndarray_z.pad ~v:Complex.zero [ [ 0; n - l ] ] nd
  else nd

let hilbert axis nd =
  let n = (N.shape nd).(axis) in
  let xf = Owl_fft_d.rfft ~axis nd in
  let xf = pad_or_sub xf n in
  let h = N.zeros Bigarray.complex64 [| n |] in
  if n mod 2 = 0 then (
    N.set h [| 0 |] Complex.one;
    N.set h [| n / 2 |] Complex.one;
    for i = 1 to n / 2 do
      N.set h [| i |] Complex.(add one one)
    done)
  else (
    N.set h [| 0 |] Complex.one;
    for i = 1 to (n + 1) / 2 do
      N.set h [| i |] Complex.(add one one)
    done);
  Owl_dense_matrix_z.(mul_ xf h);
  Owl_fft_d.irfft ~axis xfv
