open! Core
open! Async

module Constants : sig
  val program_start_location : int
end

type t

(* initialzes memory and loads font *)
val init : unit -> t

(* loads [program_file] into memory *)
val load_program : t -> program_file:string -> unit Deferred.t
val to_string_hum : t -> string
val read : t -> loc:int -> int

(* returns a hexadecimal string *)
val read_hum : t -> loc:int -> string
val write : t -> loc:int -> int -> unit

module Testing : sig
  val load_program : t -> Bytes.t -> unit
end
