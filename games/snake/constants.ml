open! Core
open! Import
include Chip_8_base.Constants

let size_of_snake = 4
let snakes_per_row = display_pixel_width / size_of_snake
let snake_sprite = [ 0xF0; 0xF0; 0xF0; 0xF0 ]

(* roughly middle of the screen, skewing slightly up and to the left *)
let snake_start_x = 7
let snake_start_y = 3
