open! Core

module Constants = struct
  let bytes_in_memory = 4096
end

module Location = struct
  include Unsigned.UInt16

  let of_int_exn int =
    try of_int int
    with exn ->
      raise_s [%message "Invalid memory location" (int : int) (exn : exn)]
end

type t = Bytes.t [@@deriving sexp]

let init () = Bytes.make Constants.bytes_in_memory '\x00'

let to_string_hum t =
  Bytes.to_list t |> List.chunks_of ~length:4
  |> List.map ~f:(fun four_chars ->
         match List.length four_chars with
         | 4 ->
             List.map four_chars ~f:(fun char ->
                 char |> int_of_char |> Unsigned.UInt8.of_int
                 |> Hexstring_helpers.of_uint8)
             |> String.concat ~sep:" "
         | _ ->
             raise_s
               [%message
                 "Invalid memory size -- expected to be divisible by 4" (t : t)])
  |> String.concat ~sep:"\n"

let read t ~loc =
  Bytes.get t (Location.to_int loc) |> int_of_char |> Unsigned.UInt8.of_int

let read_hum t ~loc = read t ~loc |> Hexstring_helpers.of_uint8

let write t ~loc int =
  let char = Unsigned.UInt8.to_int int |> char_of_int in
  Bytes.set t (Location.to_int loc) char
