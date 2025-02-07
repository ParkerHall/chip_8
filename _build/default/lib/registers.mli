open! Core

type t [@@deriving sexp_of]

val init : unit -> t

(* [read_exn t ~index ] raises if [i < 0] or [i > NUM REGISTERS] *)
val read_exn : t -> index:int -> int

(* [write_exn t ~index ] raises if [i < 0] or [i > NUM REGISTERS] *)
val write_exn : t -> index:int -> int -> unit
val set_flag_register : t -> unit
val unset_flag_register : t -> unit
