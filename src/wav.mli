type src = Cstruct.t -> int
type dst
type header

val pp_header : header Fmt.t

type reader

val header : reader -> header
val sample_size : header -> int
val sample_exn : reader -> Cstruct.t
val sample : reader -> Cstruct.t option
val raw_samples : reader -> Cstruct.buffer
val reader : ?initial_size:int -> ?max_size:int -> src -> reader
