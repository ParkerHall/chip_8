open! Core

type t

val init : unit -> t
val flip : t -> x:int -> y:int -> t
val is_set : t -> x:int -> y:int -> bool
val clear : t -> t
val run_test : unit -> unit
