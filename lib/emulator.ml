open! Core
open! Async
open! Import

module Constants = struct
  include Constants

  let keypress_frequency = Time_ns.Span.of_sec (1. /. 60.)
  let opcode_frequency = Time_ns.Span.of_sec (1. /. 480.)
  let max_8_bit_int = int_of_float (2. ** 8.) - 1
  let repeat_keypress_for_n_cycles = 5
  let timer_frequency = Time_ns.Span.of_sec (1. /. 60.)

  let decrement_timers_every_nth_opcode =
    Time_ns.Span.div timer_frequency opcode_frequency |> Int63.to_int_exn
end

module Options = struct
  type t = {
    detect_jump_self_loop : bool;
    disable_graphics : bool;
    ignore_y_on_shift : bool;
    increment_index_on_store_or_load : bool;
    jump_with_offset : [ `NNN | `XNN ];
    keypress_frequency : Time_ns.Span.t;
    repeat_keypress_for_n_cycles : int;
  }
  [@@deriving sexp_of]

  let default =
    {
      detect_jump_self_loop = false;
      disable_graphics = false;
      ignore_y_on_shift = false;
      increment_index_on_store_or_load = false;
      jump_with_offset = `NNN;
      keypress_frequency = Constants.keypress_frequency;
      repeat_keypress_for_n_cycles = Constants.repeat_keypress_for_n_cycles;
    }

  let default_for_testing =
    {
      default with
      detect_jump_self_loop = true;
      disable_graphics = true;
      (* required to pass BC test *)
      ignore_y_on_shift = true;
    }

  let flag =
    let%map_open.Command detect_jump_self_loop =
      flag "detect-jump-self-loop" no_arg
        ~doc:" if specified, halts upon encountering JUMP self-loop"
    and disable_graphics =
      flag "disable-graphics" no_arg ~doc:" if specified, disables X11 graphics"
    and ignore_y_on_shift =
      flag "ignore-y-on-shift" no_arg
        ~doc:" if specified, shift X in place and ignore Y"
    and increment_index_on_store_or_load =
      flag "increment-index-on-store-or-load" no_arg
        ~doc:
          " if specified, increments the index register during store/load \
           operations"
    and jump_with_offset =
      flag "super-chip-jump-with-offset" no_arg
        ~doc:" if specified, use SUPER-CHIP JUMP with offset semantics"
      >>| fun super_chip -> if super_chip then `XNN else `NNN
    and keypress_frequency =
      flag "keypress-frequency"
        (optional_with_default Constants.timer_frequency Time_ns.Span.arg_type)
        ~doc:"SPAN keypress frequency (defaults to 1/60s)"
    and repeat_keypress_for_n_cycles =
      flag "repeat-keypress-for-n-cycles"
        (optional_with_default Constants.repeat_keypress_for_n_cycles int)
        ~doc:"INT repeat keypress for n input cycles (defaults to 5)"
    in
    {
      detect_jump_self_loop;
      disable_graphics;
      ignore_y_on_shift;
      increment_index_on_store_or_load;
      jump_with_offset;
      keypress_frequency;
      repeat_keypress_for_n_cycles;
    }
end

module State = struct
  type t = {
    delay_timer : int;
    display : (Display.t[@sexp.opaque]);
    halt_reason : Sexp.t option;
    index_register : int; (* 16-bits *)
    keyboard_input : (Keyboard_input.t[@sexp.opaque]);
    memory : (Memory.t[@sexp.opaque]);
    options : Options.t;
    program_counter : int;
    registers : Registers.t;
    stack : int Stack.t; (* 16-bits *)
    sound_timer : int;
  }
  [@@deriving sexp_of]

  let init ~(options : Options.t) =
    let display, keyboard_input =
      match options.disable_graphics with
      | true ->
          ( Display.Testing.init_no_graphics (),
            Keyboard_input.Testing.init_no_keypresses () )
      | false ->
          let keyboard_options =
            {
              Keyboard_input.Options.frequency = options.keypress_frequency;
              repeat_keypress_for_n_cycles =
                options.repeat_keypress_for_n_cycles;
            }
          in
          (Display.init (), Keyboard_input.init keyboard_options)
    in
    {
      delay_timer = 0;
      display;
      halt_reason = None;
      index_register = 0;
      keyboard_input;
      memory = Memory.init ();
      options;
      program_counter = Constants.program_start_memory_location;
      registers = Registers.init ();
      stack = Stack.create ();
      sound_timer = 0;
    }

  let load_program t ~program_file = Memory.load_program t.memory ~program_file
  let display t = t.display

  let decrement_timers t =
    let decr value = Int.max 0 (value - 1) in
    {
      t with
      delay_timer = decr t.delay_timer;
      sound_timer = decr t.sound_timer;
    }
end

let handle_opcode' (state : State.t) (opcode : Opcode.t) =
  match opcode with
  | Add_to_index_register { index } ->
      let to_add = Registers.read_exn state.registers ~index in
      (* index register is 16-bits *)
      {
        state with
        index_register = (state.index_register + to_add) land 0xFFFF;
      }
  | Add_to_register { index; to_add } ->
      let old_value = Registers.read_exn state.registers ~index in
      let addend =
        match to_add with
        | Direct value -> value
        | Register index -> Registers.read_exn state.registers ~index
      in
      let new_value = old_value + addend in
      let () =
        match to_add with
        | Direct _ -> ()
        | Register _ -> (
            match new_value > Constants.max_8_bit_int with
            | true -> Registers.set_flag_register state.registers
            | false -> Registers.unset_flag_register state.registers)
      in
      (* note that [Registers.write_exn] will handle overflow *)
      Registers.write_exn state.registers ~index new_value;
      state
  | Binary_operation { x_index; y_index; operation } ->
      let x = Registers.read_exn state.registers ~index:x_index in
      let y = Registers.read_exn state.registers ~index:y_index in
      let new_value =
        match operation with
        | `OR -> x lor y
        | `AND -> x land y
        | `XOR -> x lxor y
      in
      Registers.write_exn state.registers ~index:x_index new_value;
      state
  | Clear_screen ->
      let display = Display.clear state.display in
      { state with display }
  | Decimal_conversion { index } ->
      let number = Registers.read_exn state.registers ~index in
      let hundreds = number / 100 in
      let tens = number / 10 % 10 in
      let ones = number % 10 in
      Memory.write state.memory ~loc:state.index_register hundreds;
      Memory.write state.memory ~loc:(state.index_register + 1) tens;
      Memory.write state.memory ~loc:(state.index_register + 2) ones;
      state
  | Draw { x_index; y_index; num_bytes } ->
      (* N.B. phall (2025-01-27): [x] coordinates are modulo'd _only_ at the beginning.
      The first coordinate always appears on screen, but future pixels that exceed the
      display boundary do not wrap. *)
      let initial_x =
        Registers.read_exn state.registers ~index:x_index
        % Constants.display_pixel_width
      in
      let initial_y = Registers.read_exn state.registers ~index:y_index in
      Registers.unset_flag_register state.registers;
      let display =
        List.init num_bytes ~f:Fn.id
        |> List.fold ~init:state.display ~f:(fun display dy ->
               let y = initial_y + dy in
               let byte =
                 let loc = state.index_register + dy in
                 Memory.read state.memory ~loc
               in
               List.init Constants.bits_in_byte ~f:Fn.id
               |> List.fold ~init:display ~f:(fun display dx ->
                      let shift = Constants.bits_in_byte - 1 - dx in
                      let nth_bit_is_set = byte land (1 lsl shift) <> 0 in
                      match nth_bit_is_set with
                      | false -> display
                      | true ->
                          let x = initial_x + dx in
                          let new_display, pixel_status =
                            let location = Display.Location.create ~x ~y in
                            Display.flip display location
                          in
                          let () =
                            match pixel_status with
                            | `Out_of_bounds | `Set -> ()
                            | `Unset ->
                                Registers.set_flag_register state.registers
                          in
                          new_display))
      in
      { state with display }
  | Get_font_character { index } ->
      (* registers store 16-bit ints but each font character is only 8 bits *)
      let hex_char = Registers.read_exn state.registers ~index land 0x0F in
      { state with index_register = Memory.Helpers.font_location ~hex_char }
  | Get_key { index } -> (
      match Keyboard_input.current_key state.keyboard_input with
      | Some key ->
          Registers.write_exn state.registers ~index
            (Keyboard_input.Key.to_int key);
          state
      | None -> { state with program_counter = state.program_counter - 2 })
  | Halt ->
      let halt_reason = [%message "Decoded empty opcode (0x0000)!"] |> Some in
      { state with halt_reason }
  | Jump { new_program_counter_base; with_offset } -> (
      let new_program_counter =
        match with_offset with
        | false -> new_program_counter_base
        | true ->
            let index =
              match state.options.jump_with_offset with
              | `XNN -> new_program_counter_base lsr 8
              | `NNN -> 0
            in
            let from_register = Registers.read_exn state.registers ~index in
            new_program_counter_base + from_register
      in
      let opcode_program_counter = state.program_counter - 2 in
      match
        state.options.detect_jump_self_loop
        && opcode_program_counter = new_program_counter
      with
      | true ->
          let halt_reason =
            [%message
              "Encountered infinite JUMP loop!"
                (opcode_program_counter : int)
                (opcode : Opcode.t)]
            |> Some
          in
          { state with halt_reason }
      | false -> { state with program_counter = new_program_counter })
  | Load { up_to_index } -> (
      List.init (up_to_index + 1) ~f:Fn.id
      |> List.iter ~f:(fun index ->
             let value =
               Memory.read state.memory ~loc:(state.index_register + index)
             in
             Registers.write_exn state.registers ~index value);
      match state.options.increment_index_on_store_or_load with
      | true ->
          {
            state with
            index_register =
              (state.index_register + up_to_index + 1) land 0xFFFF;
          }
      | false -> state)
  | Random { index; and_with } ->
      let random = Random.int (Constants.max_8_bit_int + 1) in
      Registers.write_exn state.registers ~index (random land and_with);
      state
  | Set_index_register { value } ->
      { state with index_register = value land 0xFFFF }
  | Set_register { index; to_ } ->
      let value =
        match to_ with
        | Timer Delay -> state.delay_timer
        | Timer Sound -> state.sound_timer
        | Non_timer (Direct value) -> value
        | Non_timer (Register index) ->
            Registers.read_exn state.registers ~index
      in
      Registers.write_exn state.registers ~index value;
      state
  | Set_timer { index; timer } -> (
      let value = Registers.read_exn state.registers ~index in
      match timer with
      | Delay -> { state with delay_timer = value }
      | Sound -> { state with sound_timer = value })
  | Shift { x_index; y_index; direction } ->
      let value =
        match state.options.ignore_y_on_shift with
        | true -> Registers.read_exn state.registers ~index:x_index
        | false -> Registers.read_exn state.registers ~index:y_index
      in
      let shifted, flag_bit =
        match direction with
        | `left -> (value lsl 1, value land 0x80 = 0x80)
        | `right -> (value lsr 1, value land 0x01 = 0x01)
      in
      Registers.write_exn state.registers ~index:x_index shifted;
      let () =
        match flag_bit with
        | true -> Registers.set_flag_register state.registers
        | false -> Registers.unset_flag_register state.registers
      in
      state
  | Skip_if_key { index; skip_if } ->
      let goal_key = Registers.read_exn state.registers ~index in
      let equal =
        Keyboard_input.current_key state.keyboard_input
        |> Option.value_map ~default:false ~f:(fun key ->
               Keyboard_input.Key.to_int key = goal_key)
      in
      let should_skip =
        match skip_if with Equal -> equal | Not_equal -> not equal
      in
      let program_counter =
        match should_skip with
        | true -> state.program_counter + 2
        | false -> state.program_counter
      in
      { state with program_counter }
  | Skip_if_register { left_index; right; skip_if } ->
      let left = Registers.read_exn state.registers ~index:left_index in
      let right =
        match right with
        | Direct value -> value
        | Register index -> Registers.read_exn state.registers ~index
      in
      let should_skip =
        match skip_if with Equal -> ( = ) | Not_equal -> ( <> )
      in
      let program_counter =
        match should_skip left right with
        | true -> state.program_counter + 2
        | false -> state.program_counter
      in
      { state with program_counter }
  | Store { up_to_index } -> (
      List.init (up_to_index + 1) ~f:Fn.id
      |> List.iter ~f:(fun index ->
             let value = Registers.read_exn state.registers ~index in
             Memory.write state.memory ~loc:(state.index_register + index) value);
      match state.options.increment_index_on_store_or_load with
      | true ->
          {
            state with
            index_register =
              (state.index_register + up_to_index + 1) land 0xFFFF;
          }
      | false -> state)
  | Subroutine_end ->
      (* memory addresses on the stack are 16-bits *)
      let program_counter = Stack.pop_exn state.stack land 0xFFFF in
      { state with program_counter }
  | Subroutine_start { memory_location } ->
      Stack.push state.stack state.program_counter;
      { state with program_counter = memory_location }
  | Subtract { x_index; y_index; set_x_to } ->
      let x = Registers.read_exn state.registers ~index:x_index in
      let y = Registers.read_exn state.registers ~index:y_index in
      let diff, set_flag =
        match set_x_to with
        | `x_minus_y -> (x - y, x >= y)
        | `y_minus_x -> (y - x, y >= x)
      in
      Registers.write_exn state.registers ~index:x_index diff;
      let () =
        match set_flag with
        | true -> Registers.set_flag_register state.registers
        | false -> Registers.unset_flag_register state.registers
      in
      state

let handle_opcode (state : State.t) ~opcode:raw_opcode =
  [%log.global.debug
    "decoding opcode" (state.program_counter : int) (raw_opcode : int)];
  let opcode = Opcode.decode_exn raw_opcode in
  [%log.global.debug
    "handling opcode" (state.program_counter : int) (opcode : Opcode.t)];
  handle_opcode' state opcode

let fetch (state : State.t) =
  let first_half = Memory.read state.memory ~loc:state.program_counter in
  let second_half = Memory.read state.memory ~loc:(state.program_counter + 1) in
  (first_half lsl 8) lor second_half

let run ~options ~program_file =
  let state = State.init ~options in
  let%bind () = State.load_program state ~program_file in
  (* main fetch, decode, execute loop *)
  let rec loop i (state : State.t) =
    let%bind () = Clock_ns.after Constants.opcode_frequency in
    let state, new_i =
      match i % Constants.decrement_timers_every_nth_opcode with
      | 0 -> (State.decrement_timers state, 0)
      | _ -> (state, i + 1)
    in
    match state.halt_reason with
    | Some halt_reason ->
        print_s [%message "HALTING" (halt_reason : Sexp.t)];
        return state
    | None ->
        let opcode = fetch state in
        { state with program_counter = state.program_counter + 2 }
        |> handle_opcode ~opcode |> loop new_i
  in
  (* keyboard loop *)
  Keyboard_input.loop_forever state.keyboard_input;
  loop 0 state

module Testing = struct
  module Constants = struct
    include Constants

    let num_hex_chars = 16
  end

  let display_font_opcodes =
    let start_x = 0 in
    let start_y = 0 in
    let display_hexchar_width =
      Constants.display_pixel_width / Constants.bits_in_byte
    in
    List.init Constants.num_hex_chars ~f:Fn.id
    |> List.concat_map ~f:(fun i ->
           let font_location = Memory.Helpers.font_location ~hex_char:i in
           let x = start_x + (i * Constants.bits_in_byte) + 1 in
           let y =
             start_y
             + i / display_hexchar_width
               * (Memory.Constants.bytes_per_font_char + 1)
           in
           [
             Opcode.Set_index_register { value = font_location };
             Opcode.Set_register { index = 0; to_ = Non_timer (Direct x) };
             Opcode.Set_register { index = 1; to_ = Non_timer (Direct y) };
             Opcode.Draw
               {
                 x_index = 0;
                 y_index = 1;
                 num_bytes = Memory.Constants.bytes_per_font_char;
               };
           ])

  let manually_step_opcodes opcodes =
    List.fold opcodes ~init:(State.init ~options:Options.default_for_testing)
      ~f:(fun state opcode -> handle_opcode' state opcode)
    |> return

  let load_and_run_emulator opcodes =
    let tmp_filename = Filename_unix.temp_file "opcodes" "binary" in
    let%bind () =
      Writer.with_file tmp_filename ~f:(fun writer ->
          Deferred.List.iter opcodes ~how:`Sequential ~f:(fun opcode ->
              let binary = Opcode.encode_exn opcode in
              Writer.write_byte writer (binary lsr 8);
              Writer.write_byte writer (binary land 0xFF);
              Deferred.unit))
    in
    let%bind state = run ~options:Options.default ~program_file:tmp_filename in
    let%bind () = Unix.remove tmp_filename in
    return state

  let run_test ~how opcodes =
    let%map (state : State.t) =
      match how with
      | `manual_step -> manually_step_opcodes opcodes
      | `load_and_run -> load_and_run_emulator opcodes
    in
    match state.options.disable_graphics with
    | true -> Display.Testing.dump_to_stdout state.display
    | false -> Display.freeze state.display

  let display_font ~how = run_test ~how display_font_opcodes
  let snake_testing ~how = run_test ~how Chip_8_games.Snake.opcodes
  let run = run ~options:Options.default_for_testing
end
