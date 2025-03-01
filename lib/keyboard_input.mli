open! Core

module Key : sig
  type t = N1 | N2 | N3 | N4 | Q | W | E | R | A | S | D | F | Z | X | C | V
  [@@deriving sexp_of]

  val to_int : t -> int
end

type t

val init : unit -> t
val current_key : t -> Key.t option
val loop_forever : t -> frequency:Time_ns.Span.t -> unit

module Testing : sig
  val init_no_keypresses : unit -> t
end
