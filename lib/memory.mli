open! Core

type t

val create : unit -> t
val to_string_hum : t -> string
val read : t -> loc:int -> Unsigned.UInt8.t

(* returns a hexadecimal string *)
val read_hum : t -> loc:int -> string
val write : t -> loc:int -> Unsigned.UInt8.t -> unit
