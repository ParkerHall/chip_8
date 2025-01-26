open! Core

module Location : sig
  type t

  val of_int_exn : int -> t
end

type t

val init : unit -> t
val to_string_hum : t -> string
val read : t -> loc:Location.t -> Unsigned.UInt8.t

(* returns a hexadecimal string *)
val read_hum : t -> loc:Location.t -> string
val write : t -> loc:Location.t -> Unsigned.UInt8.t -> unit
