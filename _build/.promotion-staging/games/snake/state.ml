open! Core
open! Import

module Snake_byte = struct
  type t = int

  let create direction = (1 lsl 2) lor Direction.encode direction

  let%expect_test "all possible snake bytes" =
    Direction.all
    |> List.iter ~f:(fun direction ->
           create direction
           |> Hexstring_helpers.format ~num_nibbles:2
           |> print_endline);
    [%expect {|
      04
      05
      06
      07
      |}]
end

(* The state layout is:
  -   1 byte(s): snake head index
  -   1 byte(s): snake tail index
  - 128 byte(s): snake bytes *)
module Value = struct
  type t = { offset : int; length : int } [@@deriving fields ~getters]

  let head_index = { offset = 0; length = 1 }
  let tail_index = { offset = 1; length = 1 }
  let snake = { offset = 2; length = 128 }

  let offset = function
    | `head -> head_index.offset
    | `tail -> tail_index.offset
    | `snake -> snake.offset
end

let layout = Value.[ head_index; tail_index; snake ]
let total_bytes = List.sum (module Int) layout ~f:Value.length

let%test "[offset] and [length] are correct" =
  let total_length =
    List.fold layout ~init:0 ~f:(fun expected_offset { Value.offset; length } ->
        assert (expected_offset = offset);
        expected_offset + length)
  in
  total_length = total_bytes

let%expect_test "[total_bytes]" =
  print_s [%message (total_bytes : int)];
  [%expect {| (total_bytes 130) |}]

let init =
  let set_index_register =
    Opcode_plus.Set_index_register_to_state_region { offset = 0 }
  in
  let store_in_memory =
    let set_register ~index ~value =
      Opcode.Set_register { index; to_ = Non_timer (Direct value) }
    in
    let start_index =
      Constants.snake_start_x
      + (Constants.snake_start_y * Constants.display_pixel_height)
    in
    let set_head_index =
      set_register ~index:Value.head_index.offset ~value:start_index
    in
    let set_tail_index =
      set_register ~index:Value.tail_index.offset ~value:start_index
    in
    let set_snake =
      set_register
        ~index:(Value.snake.offset + start_index)
        ~value:(Snake_byte.create Direction.init)
    in
    [
      set_head_index;
      set_tail_index;
      set_snake;
      Opcode.Store { up_to_index = total_bytes - 1 };
    ]
    |> List.map ~f:Opcode_plus.finalized
  in
  set_index_register :: store_in_memory
