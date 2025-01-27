open! Core

type t

val init : unit -> t
val flip : t -> x:int -> y:int -> t * [ `Set | `Unset | `Out_of_bounds ]
val is_set : t -> x:int -> y:int -> bool
val clear : t -> t
val run_test : unit -> unit
