open! Core
open! Import
include Chip_8_base.Constants

let scratch_bytes_for_draw = 4
let snake_sprite = [ 0xF0; 0xF0; 0xF0; 0xF0 ]

(* roughly middle of the screen, skewing slightly up and to the left *)
let snake_start_x = 28
let snake_start_y = 12
