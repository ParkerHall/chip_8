open! Core
open! Import

module Constants = struct
  include Constants

  let bytes_per_opcode = 2
  let scratch_bytes_for_draw = 8
  let snake_start_direction = `Right
  let snake_start_x = 32
  let snake_start_y = 16
end

(* The state layout is:
  - 1 byte:  x diff from current snake location for drawing next step
  - 1 byte:  y diff from current snake location for drawing next step
  - 8 bytes: diff from current snake location for drawing next step
  - 1 byte:  current snake x location
  - 1 byte:  current snake y location *)
module State = struct
  module Value = struct
    type t = { offset : int; length : int } [@@deriving fields ~getters]

    let x_diff = { offset = 0; length = 1 }
    let y_diff = { offset = 1; length = 1 }
    let byte_diff = { offset = 2; length = 8 }
    let head_x = { offset = 10; length = 1 }
    let head_y = { offset = 11; length = 5 }
  end

  let layout = Value.[ x_diff; y_diff; byte_diff; head_x; head_y ]
  let total_bytes = List.sum (module Int) layout ~f:Value.length

  let%expect_test "[total_bytes]" =
    print_s [%message (total_bytes : int)];
    [%expect {| (total_bytes 16) |}]

  let%test "[offset] and [length] are correct" =
    let total_length =
      List.fold layout ~init:0
        ~f:(fun expected_offset { Value.offset; length } ->
          assert (expected_offset = offset);
          expected_offset + length)
    in
    total_length = total_bytes
end

module Helpers = struct
  let encode_snake_direction dir =
    let x_diff, y_diff =
      match dir with `Up | `Left -> (-4, -4) | `Right | `Down -> (0, 0)
    in
    let byte_diff =
      match dir with
      | `Up -> [ 0x0F; 0x0F; 0x0F; 0x0F; 0x0F; 0x0F; 0x0F; 0x0F ]
      | `Right -> [ 0xFF; 0xFF; 0xFF; 0xFF; 0x00; 0x00; 0x00; 0x00 ]
      | `Down -> [ 0xF0; 0xF0; 0xF0; 0xF0; 0xF0; 0xF0; 0xF0; 0xF0 ]
      | `Left -> [ 0x00; 0x00; 0x00; 0x00; 0xFF; 0xFF; 0xFF; 0xFF ]
    in
    (`x_diff x_diff, `y_diff y_diff, `byte_diff byte_diff)
end

module Opcode_plus = struct
  type t =
    | Complete of Opcode.t
    | Jump of { relative_to_self : int }
    | Set_index_register_to_draw_region
    | Set_index_register_to_state_region
  [@@deriving variants]

  let finalize ts =
    let start_of_draw_region =
      (* adding 1 here allows for a blank opcode before the data memory region,
      helpfully triggering the halt behavior  *)
      let num_opcodes = List.length ts + 1 in
      Constants.program_start_memory_location
      + (num_opcodes * Constants.bytes_per_opcode)
    in
    let start_of_state_region =
      start_of_draw_region + Constants.scratch_bytes_for_draw
    in
    List.mapi ts ~f:(fun i opcode ->
        let program_counter =
          Constants.program_start_memory_location + (i * 2)
        in
        match opcode with
        | Complete opcode -> opcode
        | Jump { relative_to_self } ->
            Jump
              {
                new_program_counter_base = program_counter + relative_to_self;
                with_offset = false;
              }
        | Set_index_register_to_draw_region ->
            Set_index_register { value = start_of_draw_region }
        | Set_index_register_to_state_region ->
            Set_index_register { value = start_of_state_region })
end

module Display_title_and_wait = struct
  let pad bytes = List.map bytes ~f:(fun byte -> byte lsl 4)
  let s = [ 0b0110; 0b1001; 0b0100; 0b0010; 0b1001; 0b0110 ] |> pad
  let n = [ 0b1001; 0b1001; 0b1101; 0b1011; 0b1001; 0b1001 ] |> pad
  let a = [ 0b0110; 0b1001; 0b1001; 0b1111; 0b1001; 0b1001 ] |> pad
  let k = [ 0b1001; 0b1010; 0b1100; 0b1100; 0b1010; 0b1001 ] |> pad
  let e = [ 0b1111; 0b1000; 0b1000; 0b1111; 0b1000; 0b1111 ] |> pad
  let title = [ s; n; a; k; e ]

  let opcodes =
    let start_x =
      let total_width =
        (List.length title * Constants.bits_in_byte)
        (* ignores the second half of [e], which is whitespace *)
        - (Constants.bits_in_byte / 2)
      in
      (Constants.display_pixel_width - total_width) / 2
    in
    (* [s] is arbitrary; since all characters are the same height, any would do *)
    let y = (Constants.display_pixel_height - List.length s) / 2 in
    let display_title =
      List.concat_mapi title ~f:(fun i character ->
          let set_index_register =
            Opcode_plus.Set_index_register_to_draw_region
          in
          let set_registers =
            List.mapi character ~f:(fun index byte ->
                Opcode.Set_register { index; to_ = Non_timer (Direct byte) }
                |> Opcode_plus.Complete)
          in
          let store_into_memory =
            Opcode.Store { up_to_index = List.length character - 1 }
            |> Opcode_plus.Complete
          in
          let draw =
            let x = start_x + (i * Constants.bits_in_byte) + 1 in
            Opcode.
              [
                Set_register { index = 0; to_ = Non_timer (Direct x) };
                Set_register { index = 1; to_ = Non_timer (Direct y) };
                Draw
                  {
                    x_index = 0;
                    y_index = 1;
                    num_bytes = List.length character;
                  };
              ]
            |> List.map ~f:Opcode_plus.complete
          in
          [ [ set_index_register ]; set_registers; [ store_into_memory ]; draw ]
          |> List.concat)
    in
    let wait_and_clear =
      Opcode.[ Get_key { index = 0 }; Clear_screen ]
      |> List.map ~f:Opcode_plus.complete
    in
    display_title @ wait_and_clear
end

module Init_game_state = struct
  let opcodes =
    let `x_diff x_diff, `y_diff y_diff, `byte_diff byte_diff =
      Helpers.encode_snake_direction Constants.snake_start_direction
    in
    let set_index_register = Opcode_plus.Set_index_register_to_state_region in
    let store_in_memory =
      let set_register ~index ~value =
        Opcode.Set_register { index; to_ = Non_timer (Direct value) }
      in
      let set_x_diff = set_register ~index:0 ~value:x_diff in
      let set_y_diff = set_register ~index:1 ~value:y_diff in
      let set_byte_diffs =
        List.mapi byte_diff ~f:(fun byte i ->
            set_register ~index:(i + 2) ~value:byte)
      in
      let set_snake_x =
        set_register
          ~index:(List.length byte_diff + 2)
          ~value:Constants.snake_start_x
      in
      let set_snake_y =
        set_register
          ~index:(List.length byte_diff + 3)
          ~value:Constants.snake_start_y
      in
      [ set_x_diff; set_y_diff ] @ set_byte_diffs
      @ [
          set_snake_x;
          set_snake_y;
          Opcode.Store { up_to_index = State.total_bytes - 1 };
        ]
      |> List.map ~f:Opcode_plus.complete
    in
    set_index_register :: store_in_memory
end

module Display_snake = struct
  let _opcodes =
    let _set_index_for_state_read =
      Opcode_plus.Set_index_register_to_state_region
    in
    let _load =
      Opcode.Load { up_to_index = State.total_bytes - 1 }
      |> Opcode_plus.complete
    in
    let _set_index_for_data_write =
      Opcode_plus.Set_index_register_to_draw_region
    in
    ()
end

let opcodes =
  [ Display_title_and_wait.opcodes; Init_game_state.opcodes ]
  |> List.concat |> Opcode_plus.finalize
