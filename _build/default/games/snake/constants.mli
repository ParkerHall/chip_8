open! Core
open! Import

(** specific to snake game *)

val scratch_bytes_for_draw : int
val snake_sprite : int list
val snake_start_x : int
val snake_start_y : int

(** general to chip-8 emulator *)

val bits_in_byte : int
val bytes_per_opcode : int
val program_start_memory_location : int
val display_pixel_width : int
val display_pixel_height : int
