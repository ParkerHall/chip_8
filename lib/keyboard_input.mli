open! Core

module Key : sig
  type t = N1 | N2 | N3 | N4 | Q | W | E | R | A | S | D | F | Z | X | C | V
  [@@deriving equal, sexp_of]

  val to_int : t -> int
end

module Options : sig
  type t = { frequency : Time_ns.Span.t; repeat_keypress_for_n_cycles : int }
end

type t

val init : Options.t -> t
val current_key : t -> Key.t option
val loop_forever : t -> unit

module Testing : sig
  val init_no_keypresses : unit -> t
end
