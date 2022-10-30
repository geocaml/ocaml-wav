type src = Cstruct.t -> int
type dst
type header

val sample_rate : header -> int32

val pp_header : header Fmt.t

type reader

val header : reader -> header
val sample_size : header -> int
val raw_sample_exn : reader -> Cstruct.t
val raw_sample : reader -> Cstruct.t option
val raw_samples : reader -> Cstruct.buffer
val sample_int16_exn : reader -> int
val sample_int16 : reader -> int option

val samples_int16 :
  reader ->
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t

val reader : ?initial_size:int -> ?max_size:int -> src -> reader
val is_int8 : reader -> bool
val is_int16 : reader -> bool
val is_float32 : reader -> bool

module Signal_ext : sig
    val hilbert : 
        int ->
        (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t ->
        (float, Bigarray.float64_elt) Owl_dense_ndarray_generic.t
end
