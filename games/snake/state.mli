open! Core
open! Import

module Snake_byte : sig
  type t

  val create : Direction.t -> t
end

module Value : sig
  type t = { offset : int; length : int } [@@deriving fields ~getters]

  val head_index : t
  val tail_index : t
  val snake : t
  val offset : [ `head | `tail | `snake ] -> int
end

val total_bytes : int
val init : Opcode_plus.t list
