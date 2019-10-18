
type t [@@deriving sexp]

val is_empty : t -> bool
val contains : t -> int -> bool
val create : int -> int -> t
