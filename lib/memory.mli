open! Core
open! Async

module Constants : sig
  val bytes_per_font_char : int
  val program_start_location : int
end

type t [@@deriving sexp_of]

(* initialzes memory and loads font *)
val init : unit -> t

(* loads [program_file] into memory *)
val load_program : t -> program_file:string -> unit Deferred.t
val to_string_hum : t -> string
val read : t -> loc:int -> int

(* returns a hexadecimal string *)
val read_hum : t -> loc:int -> string
val write : t -> loc:int -> int -> unit

module Helpers : sig
  val font_location : hex_char:int -> int
end

module Testing : sig
  val load_program : t -> Bytes.t -> unit
end
