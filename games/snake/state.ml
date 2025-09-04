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
  let set_index_register ~offset =
    Opcode_plus.Set_index_register_to_state_region { offset }
  in
  let set_register ~index ~value =
    Opcode.Set_register { index; to_ = Non_timer (Direct value) }
    |> Opcode_plus.finalized
  in
  let store ~up_to_index =
    Opcode.Store { up_to_index } |> Opcode_plus.finalized
  in
  let head_start_index =
    Constants.snake_start_x
    + Constants.snake_start_y
      * (Constants.display_pixel_width / Constants.size_of_snake)
  in
  let init_snake = Snake_byte.create Direction.Right in
  (* note that we init the snake as size 2. both snake bytes start facing [Right] *)
  [
    set_register ~index:0 ~value:head_start_index;
    set_register ~index:1 ~value:(head_start_index - 1);
    set_index_register ~offset:0;
    store ~up_to_index:1;
    set_register ~index:0 ~value:init_snake;
    set_register ~index:1 ~value:init_snake;
    set_index_register ~offset:(Value.snake.offset + head_start_index - 1);
    store ~up_to_index:0;
  ]
