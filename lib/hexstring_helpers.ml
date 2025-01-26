open! Core

let hexstring_of_nibble = function
  | 0x0 -> "0"
  | 0x1 -> "1"
  | 0x2 -> "2"
  | 0x3 -> "3"
  | 0x4 -> "4"
  | 0x5 -> "5"
  | 0x6 -> "6"
  | 0x7 -> "7"
  | 0x8 -> "8"
  | 0x9 -> "9"
  | 0xa -> "A"
  | 0xb -> "B"
  | 0xc -> "C"
  | 0xd -> "D"
  | 0xe -> "E"
  | 0xf -> "F"
  | int -> raise_s [%message "Invalid hex digit" (int : int)]

let hexstring_of_int ~num_nibbles int =
  List.init num_nibbles ~f:Fn.id
  |> List.map ~f:(fun i ->
         let shift_amount = num_nibbles - 1 - i in
         let nibble = (int lsr (shift_amount * 4)) land 0xF in
         hexstring_of_nibble nibble)
  |> String.concat ~sep:""

let of_uint8 uint8 =
  Unsigned.UInt8.to_int uint8 |> hexstring_of_int ~num_nibbles:2

let of_uint16 uint16 =
  Unsigned.UInt16.to_int uint16 |> hexstring_of_int ~num_nibbles:4
