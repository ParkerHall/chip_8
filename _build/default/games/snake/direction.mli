open! Core
open! Import

type t = Up | Right | Down | Left [@@deriving enumerate, variants]

val init : t
val encode : t -> int

val movement : t -> int * int
(** [movement t] returns the [(dx, dy)] movement diffs to apply to the head of
    the snake sprite *)

module Draw_diff : sig
  type direction = t
  type t = { shift_x_by : int; shift_y_by : int; bytes : int list }

  val clear_snake : t
  val move_snake : direction -> t
end
