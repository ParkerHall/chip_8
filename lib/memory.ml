open! Core

module Constants = struct
  let bytes_in_memory = 4096
end

type t = Bytes.t [@@deriving sexp]

let create () = Bytes.make Constants.bytes_in_memory '\x00'

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

let hexstring_of_int int =
  let first = int lsr 4 |> hexstring_of_nibble in
  let second = int land 0x0F |> hexstring_of_nibble in
  first ^ second

let to_string_hum t =
  Bytes.to_list t |> List.chunks_of ~length:4
  |> List.map ~f:(fun four_chars ->
         match List.length four_chars with
         | 4 ->
             List.map four_chars ~f:(fun char ->
                 int_of_char char |> hexstring_of_int)
             |> String.concat ~sep:" "
         | _ ->
             raise_s
               [%message
                 "Invalid memory size -- expected to be divisible by 4" (t : t)])
  |> String.concat ~sep:"\n"

let read' t ~loc = Bytes.get t loc |> int_of_char
let read t ~loc = read' t ~loc |> Unsigned.UInt8.of_int
let read_hum t ~loc = read' t ~loc |> hexstring_of_int

let write t ~loc int =
  let char = Unsigned.UInt8.to_int int |> char_of_int in
  Bytes.set t loc char
