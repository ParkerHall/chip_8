open! Core
open! Import

module Constants = struct
  include Constants

  let scratch_bytes_for_draw = 8
  let snake_sprite = [ 0xF0; 0xF0; 0xF0; 0xF0 ]

  (* roughly middle of the screen, skewing slightly up and to the left *)
  let snake_start_x = 28
  let snake_start_y = 12
end

module Direction = struct
  type t = Up | Right | Down | Left [@@deriving enumerate, variants]

  let init = Left
  let encode = Variants.to_rank

  let movement = function
    | Up -> (0, 4)
    | Right -> (4, 0)
    | Down -> (0, 28)
    | Left -> (60, 0)

  module Draw_diff = struct
    type t = { shift_x_by : int; shift_y_by : int; bytes : int list }

    let get direction =
      let shift_x_by, shift_y_by =
        match direction with Up | Left -> (60, 28) | Right | Down -> (0, 0)
      in
      let bytes =
        match direction with
        | Up -> [ 0x0F; 0x0F; 0x0F; 0x0F; 0x0F; 0x0F; 0x0F; 0x0F ]
        | Right -> [ 0xFF; 0xFF; 0xFF; 0xFF; 0x00; 0x00; 0x00; 0x00 ]
        | Down -> [ 0xF0; 0xF0; 0xF0; 0xF0; 0xF0; 0xF0; 0xF0; 0xF0 ]
        | Left -> [ 0x00; 0x00; 0x00; 0x00; 0xFF; 0xFF; 0xFF; 0xFF ]
      in
      { shift_x_by; shift_y_by; bytes }
  end
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

    let direction = { offset = 0; length = 1 }
    let head_x = { offset = 1; length = 1 }
    let head_y = { offset = 2; length = 1 }
  end

  let layout = Value.[ direction; head_x; head_y ]
  let total_bytes = List.sum (module Int) layout ~f:Value.length

  let%test "[offset] and [length] are correct" =
    let total_length =
      List.fold layout ~init:0
        ~f:(fun expected_offset { Value.offset; length } ->
          assert (expected_offset = offset);
          expected_offset + length)
    in
    total_length = total_bytes

  let%expect_test "[total_bytes]" =
    print_s [%message (total_bytes : int)];
    [%expect {| (total_bytes 3) |}]
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
            Opcode_plus.Set_index_register_to_draw_region { offset = 0 }
          in
          let set_registers =
            List.mapi character ~f:(fun index byte ->
                Opcode.Set_register { index; to_ = Non_timer (Direct byte) }
                |> Opcode_plus.Finalized)
          in
          let store_into_memory =
            Opcode.Store { up_to_index = List.length character - 1 }
            |> Opcode_plus.Finalized
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
            |> List.map ~f:Opcode_plus.finalized
          in
          [ [ set_index_register ]; set_registers; [ store_into_memory ]; draw ]
          |> List.concat)
    in
    let wait_and_clear =
      Opcode.[ Get_key { index = 0 }; Clear_screen ]
      |> List.map ~f:Opcode_plus.finalized
    in
    display_title @ wait_and_clear
end

module Init_game_state = struct
  let opcodes =
    let set_index_register =
      Opcode_plus.Set_index_register_to_state_region { offset = 0 }
    in
    let store_in_memory =
      let set_register ~index ~value =
        Opcode.Set_register { index; to_ = Non_timer (Direct value) }
      in
      let set_direction =
        set_register ~index:0 ~value:Direction.(encode init)
      in
      let set_snake_x = set_register ~index:1 ~value:Constants.snake_start_x in
      let set_snake_y = set_register ~index:2 ~value:Constants.snake_start_y in
      [
        set_direction;
        set_snake_x;
        set_snake_y;
        Opcode.Store { up_to_index = State.total_bytes - 1 };
      ]
      |> List.map ~f:Opcode_plus.finalized
    in
    set_index_register :: store_in_memory
end

module Draw_snake = struct
  let opcodes =
    let write_snake_sprite_to_memory =
      let set_index_for_draw_write =
        Opcode_plus.Set_index_register_to_draw_region { offset = 0 }
      in
      let write_to_registers =
        List.mapi Constants.snake_sprite ~f:(fun index byte ->
            Opcode.Set_register { index; to_ = Non_timer (Direct byte) })
        |> List.map ~f:Opcode_plus.finalized
      in
      let write_to_memory =
        Opcode.Store { up_to_index = List.length Constants.snake_sprite - 1 }
        |> Opcode_plus.Finalized
      in
      [ [ set_index_for_draw_write ]; write_to_registers; [ write_to_memory ] ]
      |> List.concat
    in
    (* Upon the first draw of the snake, we only care to load the head coordinates *)
    let set_index_for_state_read =
      Opcode_plus.Set_index_register_to_state_region
        { offset = State.Value.head_x.offset }
    in
    let load = Opcode.Load { up_to_index = 1 } |> Opcode_plus.Finalized in
    let set_index_for_draw_read =
      Opcode_plus.Set_index_register_to_draw_region { offset = 0 }
    in
    let draw =
      Opcode.Draw
        {
          x_index = 0;
          y_index = 1;
          num_bytes = List.length Constants.snake_sprite;
        }
      |> Opcode_plus.Finalized
    in
    write_snake_sprite_to_memory
    @ [ set_index_for_state_read; load; set_index_for_draw_read; draw ]
end

module Step_snake = struct
  (* loads [head_x] to register 0 and [head_y] to register 1 *)
  let load_snake_location =
    [
      Opcode_plus.Allowed_in_subroutine.Set_index_register_to_state_region
        { offset = State.Value.head_x.offset };
      Opcode.Load { up_to_index = 1 }
      |> Opcode_plus.Allowed_in_subroutine.Finalized;
    ]

  let handle_direction direction =
    let { Direction.Draw_diff.shift_x_by; shift_y_by; bytes } =
      Direction.Draw_diff.get direction
    in
    let write_byte_diff_to_memory =
      let set_index_for_draw_write =
        Opcode_plus.Allowed_in_subroutine.Set_index_register_to_draw_region
          { offset = 0 }
      in
      let write_to_registers =
        List.mapi bytes ~f:(fun index byte ->
            Opcode.Set_register { index; to_ = Non_timer (Direct byte) }
            |> Opcode_plus.Allowed_in_subroutine.Finalized)
      in
      let write_to_memory =
        Opcode.Store { up_to_index = List.length bytes - 1 }
        |> Opcode_plus.Allowed_in_subroutine.Finalized
      in
      [ [ set_index_for_draw_write ]; write_to_registers; [ write_to_memory ] ]
      |> List.concat
    in
    let update_snake_sprite =
      let shift_x =
        Opcode.Add_to_register { index = 0; to_add = Direct shift_x_by }
        |> Opcode_plus.Allowed_in_subroutine.Finalized
      in
      let shift_y =
        Opcode.Add_to_register { index = 1; to_add = Direct shift_y_by }
        |> Opcode_plus.Allowed_in_subroutine.Finalized
      in
      let set_index_for_draw_read =
        Opcode_plus.Allowed_in_subroutine.Set_index_register_to_draw_region
          { offset = 0 }
      in
      let draw =
        Opcode.Draw { x_index = 0; y_index = 1; num_bytes = List.length bytes }
        |> Opcode_plus.Allowed_in_subroutine.Finalized
      in
      [
        load_snake_location; [ shift_x; shift_y; set_index_for_draw_read; draw ];
      ]
      |> List.concat
    in
    let update_snake_location =
      let move_x, move_y = Direction.movement direction in
      let shift_x =
        Opcode.Add_to_register { index = 0; to_add = Direct move_x }
        |> Opcode_plus.Allowed_in_subroutine.Finalized
      in
      let shift_y =
        Opcode.Add_to_register { index = 1; to_add = Direct move_y }
        |> Opcode_plus.Allowed_in_subroutine.Finalized
      in
      let set_index_for_state_write =
        Opcode_plus.Allowed_in_subroutine.Set_index_register_to_state_region
          { offset = State.Value.head_x.offset }
      in
      let write_snake_location =
        Opcode.Store { up_to_index = 1 }
        |> Opcode_plus.Allowed_in_subroutine.Finalized
      in
      [
        load_snake_location;
        [ shift_x; shift_y; set_index_for_state_write; write_snake_location ];
      ]
      |> List.concat
    in
    [ write_byte_diff_to_memory; update_snake_sprite; update_snake_location ]
    |> List.concat

  let skip_if direction =
    Opcode.Skip_if_register
      {
        left_index = 0;
        right = Direct (Direction.encode direction);
        skip_if = Opcode.Skip_if.Not_equal;
      }
    |> Opcode_plus.Finalized

  let loop =
    let set_index_for_state_read =
      Opcode_plus.Set_index_register_to_state_region
        { offset = State.Value.direction.offset }
    in
    let load = Opcode.Load { up_to_index = 0 } |> Opcode_plus.Finalized in
    let direction_handling =
      List.bind Direction.all ~f:(fun direction ->
          [
            skip_if direction;
            Opcode_plus.Subroutine_body (handle_direction direction);
          ])
    in
    [ set_index_for_state_read; load ] @ direction_handling

  let opcodes =
    let num_opcodes_in_loop = List.length loop in
    loop @ [ Opcode_plus.Jump { relative_to_self = -num_opcodes_in_loop * 2 } ]
end

let opcodes =
  [
    Display_title_and_wait.opcodes;
    Init_game_state.opcodes;
    Draw_snake.opcodes;
    Step_snake.opcodes;
  ]
  |> List.concat
  |> Opcode_plus.finalize_all_exn
       ~scratch_bytes_for_draw:Constants.scratch_bytes_for_draw

let%expect_test "dump [opcodes]" =
  List.iteri opcodes ~f:(fun i opcode ->
      let memory_location =
        Constants.program_start_memory_location
        + (i * Constants.bytes_per_opcode)
      in
      let opcode = Opcode.sexp_of_t opcode |> Sexp.to_string in
      [%string "%{memory_location#Int}: %{opcode}"] |> print_endline);
  [%expect {|
    512: (Set_index_register(value 872))
    514: (Set_register(index 0)(to_(Non_timer(Direct 96))))
    516: (Set_register(index 1)(to_(Non_timer(Direct 144))))
    518: (Set_register(index 2)(to_(Non_timer(Direct 64))))
    520: (Set_register(index 3)(to_(Non_timer(Direct 32))))
    522: (Set_register(index 4)(to_(Non_timer(Direct 144))))
    524: (Set_register(index 5)(to_(Non_timer(Direct 96))))
    526: (Store(up_to_index 5))
    528: (Set_register(index 0)(to_(Non_timer(Direct 15))))
    530: (Set_register(index 1)(to_(Non_timer(Direct 13))))
    532: (Draw(x_index 0)(y_index 1)(num_bytes 6))
    534: (Set_index_register(value 872))
    536: (Set_register(index 0)(to_(Non_timer(Direct 144))))
    538: (Set_register(index 1)(to_(Non_timer(Direct 144))))
    540: (Set_register(index 2)(to_(Non_timer(Direct 208))))
    542: (Set_register(index 3)(to_(Non_timer(Direct 176))))
    544: (Set_register(index 4)(to_(Non_timer(Direct 144))))
    546: (Set_register(index 5)(to_(Non_timer(Direct 144))))
    548: (Store(up_to_index 5))
    550: (Set_register(index 0)(to_(Non_timer(Direct 23))))
    552: (Set_register(index 1)(to_(Non_timer(Direct 13))))
    554: (Draw(x_index 0)(y_index 1)(num_bytes 6))
    556: (Set_index_register(value 872))
    558: (Set_register(index 0)(to_(Non_timer(Direct 96))))
    560: (Set_register(index 1)(to_(Non_timer(Direct 144))))
    562: (Set_register(index 2)(to_(Non_timer(Direct 144))))
    564: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    566: (Set_register(index 4)(to_(Non_timer(Direct 144))))
    568: (Set_register(index 5)(to_(Non_timer(Direct 144))))
    570: (Store(up_to_index 5))
    572: (Set_register(index 0)(to_(Non_timer(Direct 31))))
    574: (Set_register(index 1)(to_(Non_timer(Direct 13))))
    576: (Draw(x_index 0)(y_index 1)(num_bytes 6))
    578: (Set_index_register(value 872))
    580: (Set_register(index 0)(to_(Non_timer(Direct 144))))
    582: (Set_register(index 1)(to_(Non_timer(Direct 160))))
    584: (Set_register(index 2)(to_(Non_timer(Direct 192))))
    586: (Set_register(index 3)(to_(Non_timer(Direct 192))))
    588: (Set_register(index 4)(to_(Non_timer(Direct 160))))
    590: (Set_register(index 5)(to_(Non_timer(Direct 144))))
    592: (Store(up_to_index 5))
    594: (Set_register(index 0)(to_(Non_timer(Direct 39))))
    596: (Set_register(index 1)(to_(Non_timer(Direct 13))))
    598: (Draw(x_index 0)(y_index 1)(num_bytes 6))
    600: (Set_index_register(value 872))
    602: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    604: (Set_register(index 1)(to_(Non_timer(Direct 128))))
    606: (Set_register(index 2)(to_(Non_timer(Direct 128))))
    608: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    610: (Set_register(index 4)(to_(Non_timer(Direct 128))))
    612: (Set_register(index 5)(to_(Non_timer(Direct 240))))
    614: (Store(up_to_index 5))
    616: (Set_register(index 0)(to_(Non_timer(Direct 47))))
    618: (Set_register(index 1)(to_(Non_timer(Direct 13))))
    620: (Draw(x_index 0)(y_index 1)(num_bytes 6))
    622: (Get_key(index 0))
    624: Clear_screen
    626: (Set_index_register(value 880))
    628: (Set_register(index 0)(to_(Non_timer(Direct 3))))
    630: (Set_register(index 1)(to_(Non_timer(Direct 28))))
    632: (Set_register(index 2)(to_(Non_timer(Direct 12))))
    634: (Store(up_to_index 2))
    636: (Set_index_register(value 872))
    638: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    640: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    642: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    644: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    646: (Store(up_to_index 3))
    648: (Set_index_register(value 881))
    650: (Load(up_to_index 1))
    652: (Set_index_register(value 872))
    654: (Draw(x_index 0)(y_index 1)(num_bytes 4))
    656: (Set_index_register(value 880))
    658: (Load(up_to_index 0))
    660: (Skip_if_register(left_index 0)(right(Direct 0))(skip_if Not_equal))
    662: (Subroutine_start(memory_location 680))
    664: (Skip_if_register(left_index 0)(right(Direct 1))(skip_if Not_equal))
    666: (Subroutine_start(memory_location 728))
    668: (Skip_if_register(left_index 0)(right(Direct 2))(skip_if Not_equal))
    670: (Subroutine_start(memory_location 776))
    672: (Skip_if_register(left_index 0)(right(Direct 3))(skip_if Not_equal))
    674: (Subroutine_start(memory_location 824))
    676: (Jump(new_program_counter_base 656)(with_offset false))
    678: Halt
    680: (Set_index_register(value 872))
    682: (Set_register(index 0)(to_(Non_timer(Direct 15))))
    684: (Set_register(index 1)(to_(Non_timer(Direct 15))))
    686: (Set_register(index 2)(to_(Non_timer(Direct 15))))
    688: (Set_register(index 3)(to_(Non_timer(Direct 15))))
    690: (Set_register(index 4)(to_(Non_timer(Direct 15))))
    692: (Set_register(index 5)(to_(Non_timer(Direct 15))))
    694: (Set_register(index 6)(to_(Non_timer(Direct 15))))
    696: (Set_register(index 7)(to_(Non_timer(Direct 15))))
    698: (Store(up_to_index 7))
    700: (Set_index_register(value 881))
    702: (Load(up_to_index 1))
    704: (Add_to_register(index 0)(to_add(Direct 60)))
    706: (Add_to_register(index 1)(to_add(Direct 28)))
    708: (Set_index_register(value 872))
    710: (Draw(x_index 0)(y_index 1)(num_bytes 8))
    712: (Set_index_register(value 881))
    714: (Load(up_to_index 1))
    716: (Add_to_register(index 0)(to_add(Direct 0)))
    718: (Add_to_register(index 1)(to_add(Direct 4)))
    720: (Set_index_register(value 881))
    722: (Store(up_to_index 1))
    724: Subroutine_end
    726: Halt
    728: (Set_index_register(value 872))
    730: (Set_register(index 0)(to_(Non_timer(Direct 255))))
    732: (Set_register(index 1)(to_(Non_timer(Direct 255))))
    734: (Set_register(index 2)(to_(Non_timer(Direct 255))))
    736: (Set_register(index 3)(to_(Non_timer(Direct 255))))
    738: (Set_register(index 4)(to_(Non_timer(Direct 0))))
    740: (Set_register(index 5)(to_(Non_timer(Direct 0))))
    742: (Set_register(index 6)(to_(Non_timer(Direct 0))))
    744: (Set_register(index 7)(to_(Non_timer(Direct 0))))
    746: (Store(up_to_index 7))
    748: (Set_index_register(value 881))
    750: (Load(up_to_index 1))
    752: (Add_to_register(index 0)(to_add(Direct 0)))
    754: (Add_to_register(index 1)(to_add(Direct 0)))
    756: (Set_index_register(value 872))
    758: (Draw(x_index 0)(y_index 1)(num_bytes 8))
    760: (Set_index_register(value 881))
    762: (Load(up_to_index 1))
    764: (Add_to_register(index 0)(to_add(Direct 4)))
    766: (Add_to_register(index 1)(to_add(Direct 0)))
    768: (Set_index_register(value 881))
    770: (Store(up_to_index 1))
    772: Subroutine_end
    774: Halt
    776: (Set_index_register(value 872))
    778: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    780: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    782: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    784: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    786: (Set_register(index 4)(to_(Non_timer(Direct 240))))
    788: (Set_register(index 5)(to_(Non_timer(Direct 240))))
    790: (Set_register(index 6)(to_(Non_timer(Direct 240))))
    792: (Set_register(index 7)(to_(Non_timer(Direct 240))))
    794: (Store(up_to_index 7))
    796: (Set_index_register(value 881))
    798: (Load(up_to_index 1))
    800: (Add_to_register(index 0)(to_add(Direct 0)))
    802: (Add_to_register(index 1)(to_add(Direct 0)))
    804: (Set_index_register(value 872))
    806: (Draw(x_index 0)(y_index 1)(num_bytes 8))
    808: (Set_index_register(value 881))
    810: (Load(up_to_index 1))
    812: (Add_to_register(index 0)(to_add(Direct 0)))
    814: (Add_to_register(index 1)(to_add(Direct 28)))
    816: (Set_index_register(value 881))
    818: (Store(up_to_index 1))
    820: Subroutine_end
    822: Halt
    824: (Set_index_register(value 872))
    826: (Set_register(index 0)(to_(Non_timer(Direct 0))))
    828: (Set_register(index 1)(to_(Non_timer(Direct 0))))
    830: (Set_register(index 2)(to_(Non_timer(Direct 0))))
    832: (Set_register(index 3)(to_(Non_timer(Direct 0))))
    834: (Set_register(index 4)(to_(Non_timer(Direct 255))))
    836: (Set_register(index 5)(to_(Non_timer(Direct 255))))
    838: (Set_register(index 6)(to_(Non_timer(Direct 255))))
    840: (Set_register(index 7)(to_(Non_timer(Direct 255))))
    842: (Store(up_to_index 7))
    844: (Set_index_register(value 881))
    846: (Load(up_to_index 1))
    848: (Add_to_register(index 0)(to_add(Direct 60)))
    850: (Add_to_register(index 1)(to_add(Direct 28)))
    852: (Set_index_register(value 872))
    854: (Draw(x_index 0)(y_index 1)(num_bytes 8))
    856: (Set_index_register(value 881))
    858: (Load(up_to_index 1))
    860: (Add_to_register(index 0)(to_add(Direct 60)))
    862: (Add_to_register(index 1)(to_add(Direct 0)))
    864: (Set_index_register(value 881))
    866: (Store(up_to_index 1))
    868: Subroutine_end
    870: Halt
    |}]
