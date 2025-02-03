open! Core

type t =
  | Halt (* not a real opcode, but helpful for testing *)
  | Clear_screen
  | Jump of { new_program_counter : int }
  | Set_register of { index : int; value : int }
  | Add_to_register of { index : int; value : int }
  | Set_index_register of { value : int }
  | Draw of { x_index : int; y_index : int; num_bytes : int }
[@@deriving sexp_of]

let combine_nibbles nibbles =
  List.fold nibbles ~init:0 ~f:(fun acc nibble -> (acc lsl 4) lor nibble)

let encode = function
  | Halt -> 0x0000
  | Clear_screen -> 0x00E0
  | Jump { new_program_counter } -> 0x1000 lor new_program_counter
  | Set_register { index; value } -> 0x6000 lor (index lsl 8) lor value
  | Add_to_register { index; value } -> 0x7000 lor (index lsl 8) lor value
  | Set_index_register { value } -> 0xA000 lor value
  | Draw { x_index; y_index; num_bytes } ->
      0xD000 lor (x_index lsl 8) lor (y_index lsl 4) lor num_bytes

let decode_exn opcode =
  let nibble_1 = opcode lsr 12 in
  let nibble_2 = (opcode lsr 8) land 0xF in
  let nibble_3 = (opcode lsr 4) land 0xF in
  let nibble_4 = opcode land 0xF in
  match (nibble_1, nibble_2, nibble_3, nibble_4) with
  | 0x0, 0x0, 0x0, 0x0 -> Halt
  | 0x0, 0x0, 0xE, 0x0 -> Clear_screen
  | 0x1, n1, n2, n3 ->
      let new_program_counter = combine_nibbles [ n1; n2; n3 ] in
      Jump { new_program_counter }
  | 0x6, index, n1, n2 ->
      let value = combine_nibbles [ n1; n2 ] in
      Set_register { index; value }
  | 0x7, index, n1, n2 ->
      let value = combine_nibbles [ n1; n2 ] in
      Add_to_register { index; value }
  | 0xA, n1, n2, n3 ->
      let value = combine_nibbles [ n1; n2; n3 ] in
      Set_index_register { value }
  | 0xD, x_index, y_index, num_bytes -> Draw { x_index; y_index; num_bytes }
  | _ ->
      raise_s
        [%message
          "Invalid opcode"
            ~raw:(opcode : int)
            ~hex:(Hexstring_helpers.format ~num_nibbles:4 opcode : string)]
