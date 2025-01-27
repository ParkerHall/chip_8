open! Core

module Constants = struct
  let empty_char = '\x00'
  let bytes_in_memory = 4096
end

type t = Bytes.t [@@deriving sexp]

let init () = Bytes.make Constants.bytes_in_memory Constants.empty_char

let to_string_hum t =
  Bytes.to_list t |> List.chunks_of ~length:4
  |> List.map ~f:(fun four_chars ->
         match List.length four_chars with
         | 4 ->
             List.map four_chars ~f:(fun char ->
                 char |> int_of_char |> Hexstring_helpers.format ~num_nibbles:2)
             |> String.concat ~sep:" "
         | _ ->
             raise_s
               [%message
                 "Invalid memory size -- expected to be divisible by 4" (t : t)])
  |> String.concat ~sep:"\n"

let read t ~loc = Bytes.get t loc |> int_of_char
let read_hum t ~loc = read t ~loc |> Hexstring_helpers.format ~num_nibbles:2
let write t ~loc value = Bytes.set t loc (char_of_int value)
