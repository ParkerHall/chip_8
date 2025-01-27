open! Core

type t =
  | Clear_screen
  | Jump of { new_program_counter : int }
  | Set_register of { index : int; value : int }
  | Add_to_register of { index : int; value : int }
  | Set_index_register of { value : int }
  | Draw of { x_index : int; y_index : int; num_bytes : int }

val decode_exn : int -> t
