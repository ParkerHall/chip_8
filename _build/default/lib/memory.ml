open! Core
open! Async

module Constants = struct
  let empty_char = '\x00'
  let bytes_in_memory = 4096
  let bytes_per_font_char = 5
  let program_start_location = 0x200
end

type t = Bytes.t [@@deriving sexp_of]

let font =
  [
    (* 0 *)
    [ 0xF0; 0x90; 0x90; 0x90; 0xF0 ];
    (* 1 *)
    [ 0x20; 0x60; 0x20; 0x20; 0x70 ];
    (* 2 *)
    [ 0xF0; 0x10; 0xF0; 0x80; 0xF0 ];
    (* 3 *)
    [ 0xF0; 0x10; 0xF0; 0x10; 0xF0 ];
    (* 4 *)
    [ 0x90; 0x90; 0xF0; 0x10; 0x10 ];
    (* 5 *)
    [ 0xF0; 0x80; 0xF0; 0x10; 0xF0 ];
    (* 6 *)
    [ 0xF0; 0x80; 0xF0; 0x90; 0xF0 ];
    (* 7 *)
    [ 0xF0; 0x10; 0x20; 0x40; 0x40 ];
    (* 8 *)
    [ 0xF0; 0x90; 0xF0; 0x90; 0xF0 ];
    (* 9 *)
    [ 0xF0; 0x90; 0xF0; 0x10; 0xF0 ];
    (* A *)
    [ 0xF0; 0x90; 0xF0; 0x90; 0x90 ];
    (* B *)
    [ 0xE0; 0x90; 0xE0; 0x90; 0xE0 ];
    (* C *)
    [ 0xF0; 0x80; 0x80; 0x80; 0xF0 ];
    (* D *)
    [ 0xE0; 0x90; 0x90; 0x90; 0xE0 ];
    (* E *)
    [ 0xF0; 0x80; 0xF0; 0x80; 0xF0 ];
    (* F *)
    [ 0xF0; 0x80; 0xF0; 0x80; 0x80 ];
  ]

let font_bytes =
  List.concat font |> List.map ~f:char_of_int |> String.of_char_list
  |> Bytes.of_string

let load_font t =
  Bytes.blit ~src:font_bytes ~src_pos:0 ~dst:t ~dst_pos:0
    ~len:(Bytes.length font_bytes)

let init () =
  let t = Bytes.make Constants.bytes_in_memory Constants.empty_char in
  load_font t;
  t

let clear_program_memory t =
  Bytes.fill t ~pos:Constants.program_start_location
    ~len:(Bytes.length t - Constants.program_start_location)
    Constants.empty_char

let load_program' t program =
  clear_program_memory t;
  Bytes.blit ~src:program ~src_pos:0 ~dst:t
    ~dst_pos:Constants.program_start_location ~len:(Bytes.length program)

let load_program t ~program_file =
  let%map program = Reader.file_contents program_file >>| Bytes.of_string in
  load_program' t program

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

module Helpers = struct
  let font_location ~hex_char = hex_char * Constants.bytes_per_font_char
end

module Testing = struct
  let load_program = load_program'
end
