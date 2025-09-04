open! Core
open! Import

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

let state_offset = function
  | `head -> State.Value.head_index.offset
  | `tail -> State.Value.tail_index.offset

module Draw_snake = struct
  let snakes_per_row = Constants.display_pixel_width / Constants.size_of_snake
  let state_to_display_shift = Constants.size_of_snake |> Int.floor_log2

  let shift_in_place ~index ~direction ~n =
    List.init n ~f:(fun (_ : int) ->
        Opcode.Shift { x_index = index; y_index = index; direction })

  let min_scratch_index ~do_not_include =
    let do_not_include = Int.Set.of_list do_not_include in
    List.init Constants.num_registers ~f:Fn.id
    |> List.find_exn ~f:(fun i -> Set.mem do_not_include i |> not)

  (* loads index value from [state_offset], converts to display x and y coordinates,
    and stores them in [x_index] and [y_index], respectively *)
  let convert_index_to_coords ~x_index ~y_index ~state_offset =
    let scratch_index =
      min_scratch_index ~do_not_include:[ x_index; y_index ]
    in
    let set_index_for_state_read =
      Opcode_plus.Set_index_register_to_state_region { offset = state_offset }
    in
    (* loads state-space index to index 0 *)
    let load = Opcode.Load { up_to_index = 0 } in
    let copy_to_x =
      Opcode.Set_register { index = x_index; to_ = Non_timer (Register 0) }
    in
    let copy_to_y =
      Opcode.Set_register { index = y_index; to_ = Non_timer (Register 0) }
    in
    (* math:
     - AND with [snakes_per_row - 1] (equivalent to mod [snakes_per_row]) to isolate x coord
     - SHIFT to the left [state_to_display_shift] to account for state-space -> display-space transform  *)
    let convert_x =
      let set_and_value =
        Opcode.Set_register
          {
            index = scratch_index;
            to_ = Non_timer (Direct (snakes_per_row - 1));
          }
      in
      let isolate_x_coord =
        Opcode.Binary_operation
          { x_index; y_index = scratch_index; operation = `AND }
      in
      set_and_value :: isolate_x_coord
      :: shift_in_place ~index:x_index ~direction:`left
           ~n:state_to_display_shift
    in
    (* math
     - SHIFT to the right [Int.floor_log2 snakes_per_row] (equivalent to div [snakes_per_row]) to isolate y coord
     - SHIFT to the left [state_to_display_shift] to account for state-space -> display-space transform *)
    let convert_y =
      shift_in_place ~index:y_index ~direction:`right
        ~n:(Int.floor_log2 snakes_per_row)
      @ shift_in_place ~index:y_index ~direction:`left ~n:state_to_display_shift
    in
    set_index_for_state_read
    :: ([ [ load; copy_to_x; copy_to_y ]; convert_x; convert_y ]
       |> List.concat
       |> List.map ~f:Opcode_plus.finalized)

  (* Loads the coordinates of [location] into registers 0 and 1, then toggles the snake sprite
  at those corrdinates. *)
  let toggle_snake_opcodes location =
    let write_snake_sprite_to_memory =
      let write_to_registers =
        List.mapi Constants.snake_sprite ~f:(fun index byte ->
            Opcode.Set_register { index; to_ = Non_timer (Direct byte) })
        |> List.map ~f:Opcode_plus.finalized
      in
      let set_index_for_draw_write =
        Opcode_plus.Set_index_register_to_draw_region { offset = 0 }
      in
      let write_to_memory =
        Opcode.Store { up_to_index = List.length Constants.snake_sprite - 1 }
        |> Opcode_plus.Finalized
      in
      [ write_to_registers; [ set_index_for_draw_write ]; [ write_to_memory ] ]
      |> List.concat
    in
    let load_x_and_y_draw_coords =
      let state_offset = state_offset location in
      convert_index_to_coords ~x_index:0 ~y_index:1 ~state_offset
    in
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
    write_snake_sprite_to_memory @ load_x_and_y_draw_coords
    @ [ set_index_for_draw_read; draw ]
end

module Step_snake = struct
  (* loads state-space index to register 0 and snake byte to register 1 *)
  let load_snake_info endpoint =
    let load = Opcode.Load { up_to_index = 0 } |> Opcode_plus.finalized in
    let load_offset_to_1 =
      let offset =
        match endpoint with
        | `head -> State.Value.head_index.offset
        | `tail -> State.Value.tail_index.offset
      in
      [
        Opcode_plus.Set_index_register_to_state_region { offset };
        load;
        Opcode.Set_register { index = 1; to_ = Non_timer (Register 0) }
        |> Opcode_plus.finalized;
      ]
    in
    let load_snake_byte_to_0 =
      [
        Opcode_plus.Set_index_register_to_state_region
          { offset = State.Value.snake.offset };
        Opcode.Add_to_index_register { index = 1 } |> Opcode_plus.finalized;
        load;
      ]
    in
    load_offset_to_1 @ load_snake_byte_to_0

  let clear_snake_byte__offset_in_1 =
    let set_value_for_state_write =
      Opcode.Set_register { index = 0; to_ = Non_timer (Direct 0) }
      |> Opcode_plus.finalized
    in
    let set_index_for_state_write =
      [
        Opcode_plus.Set_index_register_to_state_region
          { offset = State.Value.snake.offset };
        Opcode.Add_to_index_register { index = 1 } |> Opcode_plus.finalized;
      ]
    in
    let store = Opcode.Store { up_to_index = 0 } |> Opcode_plus.finalized in
    [ [ set_value_for_state_write ]; set_index_for_state_write; [ store ] ]
    |> List.concat

  let skip_if ~left_index direction =
    Opcode.Skip_if_register
      {
        left_index;
        right = Direct (Direction.encode direction);
        skip_if = Opcode.Skip_if.Not_equal;
      }
    |> Opcode_plus.Finalized

  let transition_offset__offset_in_1_byte_in_2 =
    let handle_direction (direction : Direction.t) =
      let set_operand =
        let value =
          match direction with
          | Up | Down -> Draw_snake.snakes_per_row
          | Right | Left -> 1
        in
        Opcode.Set_register { index = 0; to_ = Non_timer (Direct value) }
      in
      let update_offset =
        match direction with
        | Up | Left ->
            Opcode.Subtract { x_index = 1; y_index = 0; set_x_to = `x_minus_y }
        | Right | Down ->
            Opcode.Add_to_register { index = 1; to_add = Register 0 }
      in
      [ set_operand; update_offset ]
      |> List.map ~f:Opcode_plus.Allowed_in_subroutine.finalized
    in
    let copy_direction_to_0 =
      [
        Opcode.Set_register { index = 0; to_ = Non_timer (Direct 0x03) };
        Opcode.Binary_operation { x_index = 0; y_index = 2; operation = `AND };
      ]
      |> List.map ~f:Opcode_plus.finalized
    in
    let direction_subroutines =
      List.bind Direction.all ~f:(fun direction ->
          [
            skip_if ~left_index:0 direction;
            Opcode_plus.Subroutine_body (handle_direction direction);
          ])
    in
    copy_direction_to_0 @ direction_subroutines

  let transition_in_state location =
    let snake_offset_in_1_byte_in_2 =
      load_snake_info location
      @ [
          Opcode.Set_register { index = 2; to_ = Non_timer (Register 0) }
          |> Opcode_plus.finalized;
        ]
    in
    let clear_if_tail =
      match location with `head -> [] | `tail -> clear_snake_byte__offset_in_1
    in
    let update_offset =
      let set_value_for_state_write =
        Opcode.Set_register { index = 0; to_ = Non_timer (Register 1) }
        |> Opcode_plus.finalized
      in
      let set_index_for_state_write =
        Opcode_plus.Set_index_register_to_state_region
          { offset = state_offset location }
      in
      let store = Opcode.Store { up_to_index = 0 } |> Opcode_plus.finalized in
      transition_offset__offset_in_1_byte_in_2
      @ [ set_value_for_state_write; set_index_for_state_write; store ]
    in
    [ snake_offset_in_1_byte_in_2; clear_if_tail; update_offset ] |> List.concat

  let loop =
    let clear_tail_display = Draw_snake.toggle_snake_opcodes `tail in
    let transition_tail_state = transition_in_state `tail in
    let transition_head_state = transition_in_state `head in
    let draw_head_display = Draw_snake.toggle_snake_opcodes `head in
    [
      clear_tail_display;
      transition_tail_state;
      transition_head_state;
      draw_head_display;
    ]
    |> List.concat

  let opcodes =
    let num_opcodes_in_loop = List.length loop in
    loop @ [ Opcode_plus.Jump { relative_to_self = -num_opcodes_in_loop * 2 } ]
end

let opcodes =
  [
    Display_title_and_wait.opcodes;
    State.init;
    (* upon startup, the snake is length 2 and we only have to draw the head and tail *)
    Draw_snake.toggle_snake_opcodes `tail;
    Draw_snake.toggle_snake_opcodes `head;
    Step_snake.opcodes;
  ]
  |> List.concat |> Opcode_plus.finalize_all_exn

let%expect_test "dump [opcodes]" =
  List.iteri opcodes ~f:(fun i opcode ->
      let memory_location =
        Constants.program_start_memory_location
        + (i * Constants.bytes_per_opcode)
      in
      let opcode = Opcode.sexp_of_t opcode |> Sexp.to_string in
      [%string "%{memory_location#Int}: %{opcode}"] |> print_endline);
  [%expect
    {|
    512: (Set_index_register(value 974))
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
    534: (Set_index_register(value 974))
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
    556: (Set_index_register(value 974))
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
    578: (Set_index_register(value 974))
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
    600: (Set_index_register(value 974))
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
    626: (Set_register(index 0)(to_(Non_timer(Direct 55))))
    628: (Set_register(index 1)(to_(Non_timer(Direct 54))))
    630: (Set_index_register(value 980))
    632: (Store(up_to_index 1))
    634: (Set_register(index 0)(to_(Non_timer(Direct 5))))
    636: (Set_register(index 1)(to_(Non_timer(Direct 5))))
    638: (Set_index_register(value 1036))
    640: (Store(up_to_index 0))
    642: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    644: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    646: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    648: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    650: (Set_index_register(value 974))
    652: (Store(up_to_index 3))
    654: (Set_index_register(value 981))
    656: (Load(up_to_index 0))
    658: (Set_register(index 0)(to_(Non_timer(Register 0))))
    660: (Set_register(index 1)(to_(Non_timer(Register 0))))
    662: (Set_register(index 2)(to_(Non_timer(Direct 15))))
    664: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    666: (Shift(x_index 0)(y_index 0)(direction left))
    668: (Shift(x_index 0)(y_index 0)(direction left))
    670: (Shift(x_index 1)(y_index 1)(direction right))
    672: (Shift(x_index 1)(y_index 1)(direction right))
    674: (Shift(x_index 1)(y_index 1)(direction right))
    676: (Shift(x_index 1)(y_index 1)(direction right))
    678: (Shift(x_index 1)(y_index 1)(direction left))
    680: (Shift(x_index 1)(y_index 1)(direction left))
    682: (Set_index_register(value 974))
    684: (Draw(x_index 0)(y_index 1)(num_bytes 4))
    686: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    688: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    690: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    692: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    694: (Set_index_register(value 974))
    696: (Store(up_to_index 3))
    698: (Set_index_register(value 980))
    700: (Load(up_to_index 0))
    702: (Set_register(index 0)(to_(Non_timer(Register 0))))
    704: (Set_register(index 1)(to_(Non_timer(Register 0))))
    706: (Set_register(index 2)(to_(Non_timer(Direct 15))))
    708: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    710: (Shift(x_index 0)(y_index 0)(direction left))
    712: (Shift(x_index 0)(y_index 0)(direction left))
    714: (Shift(x_index 1)(y_index 1)(direction right))
    716: (Shift(x_index 1)(y_index 1)(direction right))
    718: (Shift(x_index 1)(y_index 1)(direction right))
    720: (Shift(x_index 1)(y_index 1)(direction right))
    722: (Shift(x_index 1)(y_index 1)(direction left))
    724: (Shift(x_index 1)(y_index 1)(direction left))
    726: (Set_index_register(value 974))
    728: (Draw(x_index 0)(y_index 1)(num_bytes 4))
    730: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    732: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    734: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    736: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    738: (Set_index_register(value 974))
    740: (Store(up_to_index 3))
    742: (Set_index_register(value 981))
    744: (Load(up_to_index 0))
    746: (Set_register(index 0)(to_(Non_timer(Register 0))))
    748: (Set_register(index 1)(to_(Non_timer(Register 0))))
    750: (Set_register(index 2)(to_(Non_timer(Direct 15))))
    752: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    754: (Shift(x_index 0)(y_index 0)(direction left))
    756: (Shift(x_index 0)(y_index 0)(direction left))
    758: (Shift(x_index 1)(y_index 1)(direction right))
    760: (Shift(x_index 1)(y_index 1)(direction right))
    762: (Shift(x_index 1)(y_index 1)(direction right))
    764: (Shift(x_index 1)(y_index 1)(direction right))
    766: (Shift(x_index 1)(y_index 1)(direction left))
    768: (Shift(x_index 1)(y_index 1)(direction left))
    770: (Set_index_register(value 974))
    772: (Draw(x_index 0)(y_index 1)(num_bytes 4))
    774: (Set_index_register(value 981))
    776: (Load(up_to_index 0))
    778: (Set_register(index 1)(to_(Non_timer(Register 0))))
    780: (Set_index_register(value 982))
    782: (Add_to_index_register(index 1))
    784: (Load(up_to_index 0))
    786: (Set_register(index 2)(to_(Non_timer(Register 0))))
    788: (Set_register(index 0)(to_(Non_timer(Direct 0))))
    790: (Set_index_register(value 982))
    792: (Add_to_index_register(index 1))
    794: (Store(up_to_index 0))
    796: (Set_register(index 0)(to_(Non_timer(Direct 3))))
    798: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    800: (Skip_if_register(left_index 0)(right(Direct 0))(skip_if Not_equal))
    802: (Subroutine_start(memory_location 910))
    804: (Skip_if_register(left_index 0)(right(Direct 1))(skip_if Not_equal))
    806: (Subroutine_start(memory_location 918))
    808: (Skip_if_register(left_index 0)(right(Direct 2))(skip_if Not_equal))
    810: (Subroutine_start(memory_location 926))
    812: (Skip_if_register(left_index 0)(right(Direct 3))(skip_if Not_equal))
    814: (Subroutine_start(memory_location 934))
    816: (Set_register(index 0)(to_(Non_timer(Register 1))))
    818: (Set_index_register(value 981))
    820: (Store(up_to_index 0))
    822: (Set_index_register(value 980))
    824: (Load(up_to_index 0))
    826: (Set_register(index 1)(to_(Non_timer(Register 0))))
    828: (Set_index_register(value 982))
    830: (Add_to_index_register(index 1))
    832: (Load(up_to_index 0))
    834: (Set_register(index 2)(to_(Non_timer(Register 0))))
    836: (Set_register(index 0)(to_(Non_timer(Direct 3))))
    838: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    840: (Skip_if_register(left_index 0)(right(Direct 0))(skip_if Not_equal))
    842: (Subroutine_start(memory_location 942))
    844: (Skip_if_register(left_index 0)(right(Direct 1))(skip_if Not_equal))
    846: (Subroutine_start(memory_location 950))
    848: (Skip_if_register(left_index 0)(right(Direct 2))(skip_if Not_equal))
    850: (Subroutine_start(memory_location 958))
    852: (Skip_if_register(left_index 0)(right(Direct 3))(skip_if Not_equal))
    854: (Subroutine_start(memory_location 966))
    856: (Set_register(index 0)(to_(Non_timer(Register 1))))
    858: (Set_index_register(value 980))
    860: (Store(up_to_index 0))
    862: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    864: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    866: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    868: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    870: (Set_index_register(value 974))
    872: (Store(up_to_index 3))
    874: (Set_index_register(value 980))
    876: (Load(up_to_index 0))
    878: (Set_register(index 0)(to_(Non_timer(Register 0))))
    880: (Set_register(index 1)(to_(Non_timer(Register 0))))
    882: (Set_register(index 2)(to_(Non_timer(Direct 15))))
    884: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    886: (Shift(x_index 0)(y_index 0)(direction left))
    888: (Shift(x_index 0)(y_index 0)(direction left))
    890: (Shift(x_index 1)(y_index 1)(direction right))
    892: (Shift(x_index 1)(y_index 1)(direction right))
    894: (Shift(x_index 1)(y_index 1)(direction right))
    896: (Shift(x_index 1)(y_index 1)(direction right))
    898: (Shift(x_index 1)(y_index 1)(direction left))
    900: (Shift(x_index 1)(y_index 1)(direction left))
    902: (Set_index_register(value 974))
    904: (Draw(x_index 0)(y_index 1)(num_bytes 4))
    906: (Jump(new_program_counter_base 730)(with_offset false))
    908: Halt
    910: (Set_register(index 0)(to_(Non_timer(Direct 16))))
    912: (Subtract(x_index 1)(y_index 0)(set_x_to x_minus_y))
    914: Subroutine_end
    916: Halt
    918: (Set_register(index 0)(to_(Non_timer(Direct 1))))
    920: (Add_to_register(index 1)(to_add(Register 0)))
    922: Subroutine_end
    924: Halt
    926: (Set_register(index 0)(to_(Non_timer(Direct 16))))
    928: (Add_to_register(index 1)(to_add(Register 0)))
    930: Subroutine_end
    932: Halt
    934: (Set_register(index 0)(to_(Non_timer(Direct 1))))
    936: (Subtract(x_index 1)(y_index 0)(set_x_to x_minus_y))
    938: Subroutine_end
    940: Halt
    942: (Set_register(index 0)(to_(Non_timer(Direct 16))))
    944: (Subtract(x_index 1)(y_index 0)(set_x_to x_minus_y))
    946: Subroutine_end
    948: Halt
    950: (Set_register(index 0)(to_(Non_timer(Direct 1))))
    952: (Add_to_register(index 1)(to_add(Register 0)))
    954: Subroutine_end
    956: Halt
    958: (Set_register(index 0)(to_(Non_timer(Direct 16))))
    960: (Add_to_register(index 1)(to_add(Register 0)))
    962: Subroutine_end
    964: Halt
    966: (Set_register(index 0)(to_(Non_timer(Direct 1))))
    968: (Subtract(x_index 1)(y_index 0)(set_x_to x_minus_y))
    970: Subroutine_end
    972: Halt
    |}]
