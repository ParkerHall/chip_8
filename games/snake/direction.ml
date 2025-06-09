open! Core
open! Import

type t = Up | Right | Down | Left [@@deriving enumerate, variants]

let init = Left
let encode = Variants.to_rank

let movement = function
  | Up -> (0, Constants.display_pixel_height - 4)
  | Right -> (4, 0)
  | Down -> (0, 4)
  | Left -> (Constants.display_pixel_width - 4, 0)

module Draw_diff = struct
  type direction = t
  type t = { shift_x_by : int; shift_y_by : int; bytes : int list }

  let clear_snake =
    { shift_x_by = 0; shift_y_by = 0; bytes = Constants.snake_sprite }

  let move_snake direction =
    let shift_x_by, shift_y_by = movement direction in
    { shift_x_by; shift_y_by; bytes = Constants.snake_sprite }
end
