open! Core

type t =
  | Clear_screen
  | Jump of Memory.Location.t
  | Set_register of { index : Registers.Index.t; value : Registers.Value.t }
  | Add_to_register of { index : Registers.Index.t; value : Registers.Value.t }
  | Set_index_register of Memory.Location.t
  | Draw of { x : Registers.Index.t; y : Registers.Index.t; height : int }

let combine_nibbles nibbles =
  List.fold nibbles ~init:0 ~f:(fun acc nibble -> (acc lsl 4) lor nibble)

let decode_exn uint16 =
  let opcode = Unsigned.UInt16.to_int uint16 in
  let nibble_1 = opcode lsr 12 in
  let nibble_2 = (opcode lsr 8) land 0xF in
  let nibble_3 = (opcode lsr 4) land 0xF in
  let nibble_4 = opcode land 0xF in
  match (nibble_1, nibble_2, nibble_3, nibble_4) with
  | 0x0, 0x0, 0xE, 0x0 -> Clear_screen
  | 0x1, n1, n2, n3 ->
      let memory_location =
        combine_nibbles [ n1; n2; n3 ] |> Memory.Location.of_int_exn
      in
      Jump memory_location
  | 0x6, index, n1, n2 ->
      let index = Registers.Index.of_int_exn index in
      let value = combine_nibbles [ n1; n2 ] |> Registers.Value.of_int_exn in
      Set_register { index; value }
  | 0x7, index, n1, n2 ->
      let index = Registers.Index.of_int_exn index in
      let value = combine_nibbles [ n1; n2 ] |> Registers.Value.of_int_exn in
      Add_to_register { index; value }
  | 0xA, n1, n2, n3 ->
      let memory_location =
        combine_nibbles [ n1; n2; n3 ] |> Memory.Location.of_int_exn
      in
      Set_index_register memory_location
  | 0xD, x, y, height ->
      let x = Registers.Index.of_int_exn x in
      let y = Registers.Index.of_int_exn y in
      Draw { x; y; height }
  | _ ->
      raise_s
        [%message
          "Invalid opcode"
            ~raw:(opcode : int)
            ~hex:(Hexstring_helpers.of_uint16 uint16 : string)]
