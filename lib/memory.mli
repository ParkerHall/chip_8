open! Core

type t

val init : unit -> t
val to_string_hum : t -> string
val read : t -> loc:int -> int

(* returns a hexadecimal string *)
val read_hum : t -> loc:int -> string
val write : t -> loc:int -> int -> unit
