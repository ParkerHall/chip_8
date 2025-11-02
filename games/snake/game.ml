open! Core
open! Async
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
            to_ = Non_timer (Direct (Constants.snakes_per_row - 1));
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
        ~n:(Int.floor_log2 Constants.snakes_per_row)
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
    (* helpful for debugging! this verifies that snake byte loaded into register 0
    is valid (ie, first bit is 1). otherwise, the program halts. *)
    let confirm_snake_byte_valid =
      [
        Opcode.Set_register { index = 2; to_ = Non_timer (Direct 0x80) };
        Opcode.Binary_operation { x_index = 2; y_index = 0; operation = `AND };
        Opcode.Skip_if_register
          {
            left_index = 2;
            right = Direct 0x80;
            skip_if = Opcode.Skip_if.Equal;
          };
        Opcode.Halt;
      ]
      |> List.map ~f:Opcode_plus.finalized
    in
    load_offset_to_1 @ load_snake_byte_to_0 @ confirm_snake_byte_valid

  let set_snake_byte__offset_in_1 ~put_snake_byte_in_0 =
    let set_index_for_state_write =
      [
        Opcode_plus.Set_index_register_to_state_region
          { offset = State.Value.snake.offset };
        Opcode.Add_to_index_register { index = 1 } |> Opcode_plus.finalized;
      ]
    in
    let store = Opcode.Store { up_to_index = 0 } |> Opcode_plus.finalized in
    [ put_snake_byte_in_0; set_index_for_state_write; [ store ] ] |> List.concat

  let clear_snake_byte__offset_in_1 =
    let put_snake_byte_in_0 =
      [
        Opcode.Set_register { index = 0; to_ = Non_timer (Direct 0) }
        |> Opcode_plus.finalized;
      ]
    in
    set_snake_byte__offset_in_1 ~put_snake_byte_in_0

  let write_snake_byte__offset_in_1_byte_in_2 =
    let put_snake_byte_in_0 =
      [
        Opcode.Set_register { index = 0; to_ = Non_timer (Register 2) }
        |> Opcode_plus.finalized;
      ]
    in
    set_snake_byte__offset_in_1 ~put_snake_byte_in_0

  let direction_subroutines ~direction_index ~handle_direction =
    let skip_if direction =
      Opcode.Skip_if_register
        {
          left_index = direction_index;
          right = Direct (Direction.encode direction);
          skip_if = Opcode.Skip_if.Not_equal;
        }
      |> Opcode_plus.Finalized
    in
    List.bind Direction.all ~f:(fun direction ->
        [
          skip_if direction;
          Opcode_plus.Subroutine_body (handle_direction direction);
        ])

  let transition_offset__offset_in_1_byte_in_2 =
    let copy_direction_to_0 =
      [
        Opcode.Set_register { index = 0; to_ = Non_timer (Direct 0x03) };
        Opcode.Binary_operation { x_index = 0; y_index = 2; operation = `AND };
      ]
      |> List.map ~f:Opcode_plus.finalized
    in
    let direction_subroutines =
      let handle_direction (direction : Direction.t) =
        let { Direction.Movement.sign; magnitude } =
          Direction.movement direction
        in
        let set_operand =
          Opcode.Set_register { index = 0; to_ = Non_timer (Direct magnitude) }
        in
        let update_offset =
          match sign with
          | `Neg ->
              Opcode.Subtract
                { x_index = 1; y_index = 0; set_x_to = `x_minus_y }
          | `Pos -> Opcode.Add_to_register { index = 1; to_add = Register 0 }
        in
        [ set_operand; update_offset ]
        |> List.map ~f:Opcode_plus.Allowed_in_subroutine.finalized
      in
      direction_subroutines ~direction_index:0 ~handle_direction
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
    let set_if_head =
      match location with
      | `head -> write_snake_byte__offset_in_1_byte_in_2
      | `tail -> []
    in
    [ snake_offset_in_1_byte_in_2; clear_if_tail; update_offset; set_if_head ]
    |> List.concat

  let register_keypress ~input_index ~output_index =
    let value_to_direction =
      [
        (0x5, Direction.Up);
        (0x7, Direction.Left);
        (0x8, Direction.Down);
        (0x9, Direction.Right);
      ]
    in
    List.bind value_to_direction ~f:(fun (key, direction) ->
        let value = State.Snake_byte.(create direction |> to_int) in
        [
          Opcode.Set_register
            { index = input_index; to_ = Non_timer (Direct key) };
          Opcode.Skip_if_key { index = input_index; skip_if = Not_equal };
          Opcode.Set_register
            { index = output_index; to_ = Non_timer (Direct value) };
        ])
    |> List.map ~f:Opcode_plus.finalized

  let maybe_update_direction =
    let snake_offset_in_1_byte_in_2 =
      load_snake_info `head
      @ [
          Opcode.Set_register { index = 2; to_ = Non_timer (Register 0) }
          |> Opcode_plus.finalized;
        ]
    in
    let set_index_for_state_write =
      [
        Opcode_plus.Set_index_register_to_state_region
          { offset = State.Value.snake.offset };
        Opcode.Add_to_index_register { index = 1 } |> Opcode_plus.finalized;
      ]
    in
    let set_value_for_state_write =
      Opcode.Set_register { index = 0; to_ = Non_timer (Register 2) }
    in
    let update_byte_at_offset = Opcode.Store { up_to_index = 0 } in
    snake_offset_in_1_byte_in_2
    @ register_keypress ~input_index:0 ~output_index:2
    @ set_index_for_state_write
    @ ([ set_value_for_state_write; update_byte_at_offset ]
      |> List.map ~f:Opcode_plus.finalized)

  let loop =
    let clear_tail_display = Draw_snake.toggle_snake_opcodes `tail in
    let transition_tail_state = transition_in_state `tail in
    let transition_head_state = transition_in_state `head in
    let draw_head_display = Draw_snake.toggle_snake_opcodes `head in
    [
      clear_tail_display;
      transition_tail_state;
      maybe_update_direction;
      transition_head_state;
      draw_head_display;
    ]
    |> List.concat

  let opcodes =
    let num_opcodes_in_loop = List.length loop in
    loop @ [ Opcode_plus.Jump { relative_to_self = -num_opcodes_in_loop * 2 } ]
end

let gen_opcodes () =
  [
    Display_title_and_wait.opcodes;
    State.init Direction.Down;
    (* upon startup, the snake is length 2 and we only have to draw the head and tail *)
    Draw_snake.toggle_snake_opcodes `tail;
    Draw_snake.toggle_snake_opcodes `head;
    Step_snake.opcodes;
  ]
  |> List.concat |> Opcode_plus.finalize_all_exn

let opcodes = gen_opcodes ()

let setup_logging_and_opcodes () =
  let time_source =
    Synchronous_time_source.create ~now:Time_ns.epoch ()
    |> Synchronous_time_source.read_only
  in
  Log.Global.set_time_source time_source;
  Log.Global.set_level `Debug;
  let opcodes = gen_opcodes () in
  let%map () = Log.Global.flushed () in
  opcodes

let%expect_test "dump [opcodes]" =
  let%bind opcodes = setup_logging_and_opcodes () in
  [%expect
    {| 1969-12-31 19:00:00.000000-05:00 Debug ("finalizing opcodes"(start_of_draw_region 1054)(start_of_state_region 1060)) |}];
  List.iteri opcodes ~f:(fun i opcode ->
      let memory_location =
        Constants.program_start_memory_location
        + (i * Constants.bytes_per_opcode)
      in
      let opcode = Opcode.sexp_of_t opcode |> Sexp.to_string in
      [%string "%{memory_location#Int}: %{opcode}"] |> print_endline);
  [%expect
    {|
    512: (Set_index_register(value 1054))
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
    534: (Set_index_register(value 1054))
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
    556: (Set_index_register(value 1054))
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
    578: (Set_index_register(value 1054))
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
    600: (Set_index_register(value 1054))
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
    628: (Set_register(index 1)(to_(Non_timer(Direct 39))))
    630: (Set_index_register(value 1060))
    632: (Store(up_to_index 1))
    634: (Set_register(index 0)(to_(Non_timer(Direct 130))))
    636: (Set_index_register(value 1117))
    638: (Store(up_to_index 0))
    640: (Set_index_register(value 1101))
    642: (Store(up_to_index 0))
    644: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    646: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    648: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    650: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    652: (Set_index_register(value 1054))
    654: (Store(up_to_index 3))
    656: (Set_index_register(value 1061))
    658: (Load(up_to_index 0))
    660: (Set_register(index 0)(to_(Non_timer(Register 0))))
    662: (Set_register(index 1)(to_(Non_timer(Register 0))))
    664: (Set_register(index 2)(to_(Non_timer(Direct 15))))
    666: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    668: (Shift(x_index 0)(y_index 0)(direction left))
    670: (Shift(x_index 0)(y_index 0)(direction left))
    672: (Shift(x_index 1)(y_index 1)(direction right))
    674: (Shift(x_index 1)(y_index 1)(direction right))
    676: (Shift(x_index 1)(y_index 1)(direction right))
    678: (Shift(x_index 1)(y_index 1)(direction right))
    680: (Shift(x_index 1)(y_index 1)(direction left))
    682: (Shift(x_index 1)(y_index 1)(direction left))
    684: (Set_index_register(value 1054))
    686: (Draw(x_index 0)(y_index 1)(num_bytes 4))
    688: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    690: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    692: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    694: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    696: (Set_index_register(value 1054))
    698: (Store(up_to_index 3))
    700: (Set_index_register(value 1060))
    702: (Load(up_to_index 0))
    704: (Set_register(index 0)(to_(Non_timer(Register 0))))
    706: (Set_register(index 1)(to_(Non_timer(Register 0))))
    708: (Set_register(index 2)(to_(Non_timer(Direct 15))))
    710: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    712: (Shift(x_index 0)(y_index 0)(direction left))
    714: (Shift(x_index 0)(y_index 0)(direction left))
    716: (Shift(x_index 1)(y_index 1)(direction right))
    718: (Shift(x_index 1)(y_index 1)(direction right))
    720: (Shift(x_index 1)(y_index 1)(direction right))
    722: (Shift(x_index 1)(y_index 1)(direction right))
    724: (Shift(x_index 1)(y_index 1)(direction left))
    726: (Shift(x_index 1)(y_index 1)(direction left))
    728: (Set_index_register(value 1054))
    730: (Draw(x_index 0)(y_index 1)(num_bytes 4))
    732: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    734: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    736: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    738: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    740: (Set_index_register(value 1054))
    742: (Store(up_to_index 3))
    744: (Set_index_register(value 1061))
    746: (Load(up_to_index 0))
    748: (Set_register(index 0)(to_(Non_timer(Register 0))))
    750: (Set_register(index 1)(to_(Non_timer(Register 0))))
    752: (Set_register(index 2)(to_(Non_timer(Direct 15))))
    754: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    756: (Shift(x_index 0)(y_index 0)(direction left))
    758: (Shift(x_index 0)(y_index 0)(direction left))
    760: (Shift(x_index 1)(y_index 1)(direction right))
    762: (Shift(x_index 1)(y_index 1)(direction right))
    764: (Shift(x_index 1)(y_index 1)(direction right))
    766: (Shift(x_index 1)(y_index 1)(direction right))
    768: (Shift(x_index 1)(y_index 1)(direction left))
    770: (Shift(x_index 1)(y_index 1)(direction left))
    772: (Set_index_register(value 1054))
    774: (Draw(x_index 0)(y_index 1)(num_bytes 4))
    776: (Set_index_register(value 1061))
    778: (Load(up_to_index 0))
    780: (Set_register(index 1)(to_(Non_timer(Register 0))))
    782: (Set_index_register(value 1062))
    784: (Add_to_index_register(index 1))
    786: (Load(up_to_index 0))
    788: (Set_register(index 2)(to_(Non_timer(Direct 128))))
    790: (Binary_operation(x_index 2)(y_index 0)(operation AND))
    792: (Skip_if_register(left_index 2)(right(Direct 128))(skip_if Equal))
    794: Halt
    796: (Set_register(index 2)(to_(Non_timer(Register 0))))
    798: (Set_register(index 0)(to_(Non_timer(Direct 0))))
    800: (Set_index_register(value 1062))
    802: (Add_to_index_register(index 1))
    804: (Store(up_to_index 0))
    806: (Set_register(index 0)(to_(Non_timer(Direct 3))))
    808: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    810: (Skip_if_register(left_index 0)(right(Direct 0))(skip_if Not_equal))
    812: (Subroutine_start(memory_location 990))
    814: (Skip_if_register(left_index 0)(right(Direct 1))(skip_if Not_equal))
    816: (Subroutine_start(memory_location 998))
    818: (Skip_if_register(left_index 0)(right(Direct 2))(skip_if Not_equal))
    820: (Subroutine_start(memory_location 1006))
    822: (Skip_if_register(left_index 0)(right(Direct 3))(skip_if Not_equal))
    824: (Subroutine_start(memory_location 1014))
    826: (Set_register(index 0)(to_(Non_timer(Register 1))))
    828: (Set_index_register(value 1061))
    830: (Store(up_to_index 0))
    832: (Set_index_register(value 1060))
    834: (Load(up_to_index 0))
    836: (Set_register(index 1)(to_(Non_timer(Register 0))))
    838: (Set_index_register(value 1062))
    840: (Add_to_index_register(index 1))
    842: (Load(up_to_index 0))
    844: (Set_register(index 2)(to_(Non_timer(Direct 128))))
    846: (Binary_operation(x_index 2)(y_index 0)(operation AND))
    848: (Skip_if_register(left_index 2)(right(Direct 128))(skip_if Equal))
    850: Halt
    852: (Set_register(index 2)(to_(Non_timer(Register 0))))
    854: (Set_register(index 0)(to_(Non_timer(Direct 5))))
    856: (Skip_if_key(index 0)(skip_if Not_equal))
    858: (Set_register(index 2)(to_(Non_timer(Direct 128))))
    860: (Set_register(index 0)(to_(Non_timer(Direct 7))))
    862: (Skip_if_key(index 0)(skip_if Not_equal))
    864: (Set_register(index 2)(to_(Non_timer(Direct 131))))
    866: (Set_register(index 0)(to_(Non_timer(Direct 8))))
    868: (Skip_if_key(index 0)(skip_if Not_equal))
    870: (Set_register(index 2)(to_(Non_timer(Direct 130))))
    872: (Set_register(index 0)(to_(Non_timer(Direct 9))))
    874: (Skip_if_key(index 0)(skip_if Not_equal))
    876: (Set_register(index 2)(to_(Non_timer(Direct 129))))
    878: (Set_index_register(value 1062))
    880: (Add_to_index_register(index 1))
    882: (Set_register(index 0)(to_(Non_timer(Register 2))))
    884: (Store(up_to_index 0))
    886: (Set_index_register(value 1060))
    888: (Load(up_to_index 0))
    890: (Set_register(index 1)(to_(Non_timer(Register 0))))
    892: (Set_index_register(value 1062))
    894: (Add_to_index_register(index 1))
    896: (Load(up_to_index 0))
    898: (Set_register(index 2)(to_(Non_timer(Direct 128))))
    900: (Binary_operation(x_index 2)(y_index 0)(operation AND))
    902: (Skip_if_register(left_index 2)(right(Direct 128))(skip_if Equal))
    904: Halt
    906: (Set_register(index 2)(to_(Non_timer(Register 0))))
    908: (Set_register(index 0)(to_(Non_timer(Direct 3))))
    910: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    912: (Skip_if_register(left_index 0)(right(Direct 0))(skip_if Not_equal))
    914: (Subroutine_start(memory_location 1022))
    916: (Skip_if_register(left_index 0)(right(Direct 1))(skip_if Not_equal))
    918: (Subroutine_start(memory_location 1030))
    920: (Skip_if_register(left_index 0)(right(Direct 2))(skip_if Not_equal))
    922: (Subroutine_start(memory_location 1038))
    924: (Skip_if_register(left_index 0)(right(Direct 3))(skip_if Not_equal))
    926: (Subroutine_start(memory_location 1046))
    928: (Set_register(index 0)(to_(Non_timer(Register 1))))
    930: (Set_index_register(value 1060))
    932: (Store(up_to_index 0))
    934: (Set_register(index 0)(to_(Non_timer(Register 2))))
    936: (Set_index_register(value 1062))
    938: (Add_to_index_register(index 1))
    940: (Store(up_to_index 0))
    942: (Set_register(index 0)(to_(Non_timer(Direct 240))))
    944: (Set_register(index 1)(to_(Non_timer(Direct 240))))
    946: (Set_register(index 2)(to_(Non_timer(Direct 240))))
    948: (Set_register(index 3)(to_(Non_timer(Direct 240))))
    950: (Set_index_register(value 1054))
    952: (Store(up_to_index 3))
    954: (Set_index_register(value 1060))
    956: (Load(up_to_index 0))
    958: (Set_register(index 0)(to_(Non_timer(Register 0))))
    960: (Set_register(index 1)(to_(Non_timer(Register 0))))
    962: (Set_register(index 2)(to_(Non_timer(Direct 15))))
    964: (Binary_operation(x_index 0)(y_index 2)(operation AND))
    966: (Shift(x_index 0)(y_index 0)(direction left))
    968: (Shift(x_index 0)(y_index 0)(direction left))
    970: (Shift(x_index 1)(y_index 1)(direction right))
    972: (Shift(x_index 1)(y_index 1)(direction right))
    974: (Shift(x_index 1)(y_index 1)(direction right))
    976: (Shift(x_index 1)(y_index 1)(direction right))
    978: (Shift(x_index 1)(y_index 1)(direction left))
    980: (Shift(x_index 1)(y_index 1)(direction left))
    982: (Set_index_register(value 1054))
    984: (Draw(x_index 0)(y_index 1)(num_bytes 4))
    986: (Jump(new_program_counter_base 732)(with_offset false))
    988: Halt
    990: (Set_register(index 0)(to_(Non_timer(Direct 16))))
    992: (Subtract(x_index 1)(y_index 0)(set_x_to x_minus_y))
    994: Subroutine_end
    996: Halt
    998: (Set_register(index 0)(to_(Non_timer(Direct 1))))
    1000: (Add_to_register(index 1)(to_add(Register 0)))
    1002: Subroutine_end
    1004: Halt
    1006: (Set_register(index 0)(to_(Non_timer(Direct 16))))
    1008: (Add_to_register(index 1)(to_add(Register 0)))
    1010: Subroutine_end
    1012: Halt
    1014: (Set_register(index 0)(to_(Non_timer(Direct 1))))
    1016: (Subtract(x_index 1)(y_index 0)(set_x_to x_minus_y))
    1018: Subroutine_end
    1020: Halt
    1022: (Set_register(index 0)(to_(Non_timer(Direct 16))))
    1024: (Subtract(x_index 1)(y_index 0)(set_x_to x_minus_y))
    1026: Subroutine_end
    1028: Halt
    1030: (Set_register(index 0)(to_(Non_timer(Direct 1))))
    1032: (Add_to_register(index 1)(to_add(Register 0)))
    1034: Subroutine_end
    1036: Halt
    1038: (Set_register(index 0)(to_(Non_timer(Direct 16))))
    1040: (Add_to_register(index 1)(to_add(Register 0)))
    1042: Subroutine_end
    1044: Halt
    1046: (Set_register(index 0)(to_(Non_timer(Direct 1))))
    1048: (Subtract(x_index 1)(y_index 0)(set_x_to x_minus_y))
    1050: Subroutine_end
    1052: Halt
    |}]
  |> return
