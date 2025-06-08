open! Core
open! Import

module Value : sig
  type t = { offset : int; length : int } [@@deriving fields ~getters]

  val direction : t
  val head_x : t
  val head_y : t
end

val total_bytes : int
