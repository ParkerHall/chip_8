open! Core

module Timer : sig
  type t = Delay | Sound
end

module Value_source : sig
  module Non_timer : sig
    type t = Direct of int | Register of int
  end

  type t = Non_timer of Non_timer.t | Timer of Timer.t
end

module Skip_if : sig
  type t = Equal | Not_equal
end

type t =
  | Add_to_index_register of { index : int }
  | Add_to_register of { index : int; to_add : Value_source.Non_timer.t }
  | Binary_operation of {
      x_index : int;
      y_index : int;
      operation : [ `OR | `AND | `XOR ];
    }
  | Clear_screen
  | Decimal_conversion of { index : int }
  | Draw of { x_index : int; y_index : int; num_bytes : int }
  | Get_font_character of { index : int }
  | Get_key of { index : int }
  | Halt (* not a real opcode, but helpful for testing *)
  | Jump of { new_program_counter : int; with_offset : bool }
  | Load of { up_to_index : int }
  | Random of { index : int; and_with : int }
  | Set_index_register of { value : int }
  | Set_register of { index : int; to_ : Value_source.t }
  | Set_timer of { index : int; timer : Timer.t }
  | Shift of { x_index : int; y_index : int; direction : [ `left | `right ] }
  | Skip_if_register of {
      left_index : int;
      right : Value_source.Non_timer.t;
      skip_if : Skip_if.t;
    }
  | Skip_if_key of { index : int; skip_if : Skip_if.t }
  | Store of { up_to_index : int }
  | Subroutine_end
  | Subroutine_start of { memory_location : int }
  | Subtract of {
      x_index : int;
      y_index : int;
      set_x_to : [ `x_minus_y | `y_minus_x ];
    }
[@@deriving sexp_of]

val encode_exn : t -> int
val decode_exn : int -> t
