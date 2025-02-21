open! Core

module Timer = struct
  type t = Delay | Sound [@@deriving sexp_of]
end

module Value_source = struct
  module Non_timer = struct
    type t = Direct of int | Register of int [@@deriving sexp_of]

    let to_nibbles = function
      | Direct value -> [ value lsr 4; value land 0x0F ]
      | Register index -> [ index; 0x0 ]
  end

  type t = Non_timer of Non_timer.t | Timer of Timer.t [@@deriving sexp_of]
end

module Skip_if = struct
  type t = Equal | Not_equal [@@deriving sexp_of]
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

let combine_nibbles nibbles =
  List.fold nibbles ~init:0 ~f:(fun acc nibble -> (acc lsl 4) lor nibble)

let encode_exn = function
  | Add_to_index_register { index } -> combine_nibbles [ 0xF; index; 0x1; 0xE ]
  | Add_to_register { index; to_add } ->
      let n1 = match to_add with Direct _ -> 0x7 | Register _ -> 0x8 in
      let n34 = Value_source.Non_timer.to_nibbles to_add in
      n1 :: index :: n34 |> combine_nibbles
  | Binary_operation { x_index; y_index; operation } ->
      let n4 = match operation with `OR -> 0x1 | `AND -> 0x2 | `XOR -> -0x3 in
      combine_nibbles [ 0x8; x_index; y_index; n4 ]
  | Clear_screen -> 0x00E0
  | Decimal_conversion { index } -> combine_nibbles [ 0xF; index; 0x3; 0x3 ]
  | Draw { x_index; y_index; num_bytes } ->
      combine_nibbles [ 0xD; x_index; y_index; num_bytes ]
  | Get_font_character { index } -> combine_nibbles [ 0xF; index; 0x2; 0x9 ]
  | Get_key { index } -> combine_nibbles [ 0xF; index; 0x0; 0xA ]
  | Halt -> 0x0000
  | Jump { new_program_counter; with_offset } ->
      let n1 = if with_offset then 0xB else 0x1 in
      (n1 lsl 12) lor new_program_counter
  | Load { up_to_index } -> combine_nibbles [ 0xF; up_to_index; 0x6; 0x5 ]
  | Random { index; and_with } -> 0xC000 lor (index lsl 8) lor and_with
  | Set_index_register { value } -> 0xA000 lor value
  | Set_register { index; to_ } as t ->
      let n1 =
        match to_ with
        | Timer Sound ->
            raise_s [%message "Attempted to encode an invalid opcode" (t : t)]
        | Timer Delay -> 0xF
        | Non_timer (Direct _) -> 0x6
        | Non_timer (Register _) -> 0x8
      in
      let n34 =
        match to_ with
        | Timer _ -> [ 0x0; 0x7 ]
        | Non_timer non_timer -> Value_source.Non_timer.to_nibbles non_timer
      in
      n1 :: index :: n34 |> combine_nibbles
  | Set_timer { index; timer } ->
      let n4 = match timer with Delay -> 0x5 | Sound -> 0x8 in
      combine_nibbles [ 0xF; index; 0x1; n4 ]
  | Shift { x_index; y_index; direction } ->
      let n4 = match direction with `right -> 0x6 | `left -> 0xE in
      combine_nibbles [ 0x8; x_index; y_index; n4 ]
  | Skip_if_key { index; skip_if } ->
      let n34 =
        match skip_if with Equal -> [ 0x9; 0xE ] | Not_equal -> [ 0xA; 0x1 ]
      in
      combine_nibbles (0xE :: index :: n34)
  | Skip_if_register { left_index; right; skip_if } ->
      let n1 =
        match (right, skip_if) with
        | Direct _, Equal -> 0x3
        | Direct _, Not_equal -> 0x4
        | Register _, Equal -> 0x5
        | Register _, Not_equal -> 0x9
      in
      let n34 = Value_source.Non_timer.to_nibbles right in
      n1 :: left_index :: n34 |> combine_nibbles
  | Store { up_to_index } -> combine_nibbles [ 0xF; up_to_index; 0x5; 0x5 ]
  | Subroutine_end -> 0x00EE
  | Subroutine_start { memory_location } -> 0x2000 lor memory_location
  | Subtract { x_index; y_index; set_x_to } ->
      let n4 = match set_x_to with `x_minus_y -> 0x5 | `y_minus_x -> 0x7 in
      combine_nibbles [ 0x8; x_index; y_index; n4 ]

let decode_exn opcode =
  let nibble_1 = opcode lsr 12 in
  let nibble_2 = (opcode lsr 8) land 0xF in
  let nibble_3 = (opcode lsr 4) land 0xF in
  let nibble_4 = opcode land 0xF in
  match (nibble_1, nibble_2, nibble_3, nibble_4) with
  | 0x0, 0x0, 0x0, 0x0 -> Halt
  | 0x0, 0x0, 0xE, 0x0 -> Clear_screen
  | 0x0, 0x0, 0xE, 0xE -> Subroutine_end
  | 0x1, n1, n2, n3 ->
      let new_program_counter = combine_nibbles [ n1; n2; n3 ] in
      Jump { new_program_counter; with_offset = false }
  | 0x2, n1, n2, n3 ->
      let memory_location = combine_nibbles [ n1; n2; n3 ] in
      Subroutine_start { memory_location }
  | 0x3, x, n1, n2 ->
      let value = combine_nibbles [ n1; n2 ] in
      Skip_if_register { left_index = x; right = Direct value; skip_if = Equal }
  | 0x4, x, n1, n2 ->
      let value = combine_nibbles [ n1; n2 ] in
      Skip_if_register
        { left_index = x; right = Direct value; skip_if = Not_equal }
  | 0x5, x, y, 0x0 ->
      Skip_if_register { left_index = x; right = Register y; skip_if = Equal }
  | 0x6, index, n1, n2 ->
      let value = combine_nibbles [ n1; n2 ] in
      Set_register { index; to_ = Non_timer (Direct value) }
  | 0x7, index, n1, n2 ->
      let to_add = combine_nibbles [ n1; n2 ] in
      Add_to_register { index; to_add = Direct to_add }
  | 0x8, index, to_, 0x0 ->
      Set_register { index; to_ = Non_timer (Register to_) }
  | 0x8, x_index, y_index, 0x1 ->
      Binary_operation { x_index; y_index; operation = `OR }
  | 0x8, x_index, y_index, 0x2 ->
      Binary_operation { x_index; y_index; operation = `AND }
  | 0x8, x_index, y_index, 0x3 ->
      Binary_operation { x_index; y_index; operation = `XOR }
  | 0x8, index, to_add, 0x4 ->
      Add_to_register { index; to_add = Register to_add }
  | 0x8, x_index, y_index, 0x5 ->
      Subtract { x_index; y_index; set_x_to = `x_minus_y }
  | 0x8, x_index, y_index, 0x6 -> Shift { x_index; y_index; direction = `right }
  | 0x8, x_index, y_index, 0x7 ->
      Subtract { x_index; y_index; set_x_to = `y_minus_x }
  | 0x8, x_index, y_index, 0xE -> Shift { x_index; y_index; direction = `left }
  | 0x9, x, y, 0x0 ->
      Skip_if_register
        { left_index = x; right = Register y; skip_if = Not_equal }
  | 0xA, n1, n2, n3 ->
      let value = combine_nibbles [ n1; n2; n3 ] in
      Set_index_register { value }
  | 0xB, n1, n2, n3 ->
      let new_program_counter = combine_nibbles [ n1; n2; n3 ] in
      Jump { new_program_counter; with_offset = true }
  | 0xC, index, n1, n2 ->
      let and_with = combine_nibbles [ n1; n2 ] in
      Random { index; and_with }
  | 0xD, x_index, y_index, num_bytes -> Draw { x_index; y_index; num_bytes }
  | 0xE, index, 0x9, 0xE -> Skip_if_key { index; skip_if = Equal }
  | 0xE, index, 0xA, 0x1 -> Skip_if_key { index; skip_if = Not_equal }
  | 0xF, index, 0x0, 0x7 -> Set_register { index : int; to_ = Timer Delay }
  | 0xF, index, 0x0, 0xA -> Get_key { index }
  | 0xF, index, 0x1, 0x5 -> Set_timer { index : int; timer = Delay }
  | 0xF, index, 0x1, 0x8 -> Set_timer { index : int; timer = Sound }
  | 0xF, index, 0x1, 0xE -> Add_to_index_register { index }
  | 0xF, index, 0x2, 0x9 -> Get_font_character { index }
  | 0xF, index, 0x3, 0x3 -> Decimal_conversion { index }
  | 0xF, up_to_index, 0x5, 0x5 -> Store { up_to_index }
  | 0xF, up_to_index, 0x6, 0x5 -> Load { up_to_index }
  | _ ->
      raise_s
        [%message
          "Invalid opcode"
            ~raw:(opcode : int)
            ~hex:(Hexstring_helpers.format ~num_nibbles:4 opcode : string)]
