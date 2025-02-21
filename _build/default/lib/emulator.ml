open! Core
open! Async

module Constants = struct
  let bits_in_byte = 8
end

module Options = struct
  type t = {
    detect_jump_self_loop : bool;
    disable_graphics : bool;
    jump_with_offset : [ `NNN | `XNN ];
  }
  [@@deriving sexp_of]

  let default_for_testing =
    {
      detect_jump_self_loop = true;
      disable_graphics = true;
      jump_with_offset = `NNN;
    }

  let default =
    {
      detect_jump_self_loop = false;
      disable_graphics = false;
      jump_with_offset = `NNN;
    }

  let flag =
    let%map_open.Command detect_jump_self_loop =
      flag "detect-jump-self-loop" no_arg
        ~doc:" if specified, halts upon encountering JUMP self-loop"
    and disable_graphics =
      flag "disable-graphics" no_arg ~doc:" if specified, disables X11 graphics"
    and jump_with_offset =
      flag "super-chip-jump-with-offset" no_arg
        ~doc:" if specified, use SUPER-CHIP JUMP with offset semantics"
      >>| fun super_chip -> if super_chip then `XNN else `NNN
    in
    { detect_jump_self_loop; disable_graphics; jump_with_offset }
end

module State = struct
  type t = {
    mutable delay_timer : int;
    display : (Display.t[@sexp.opaque]);
    halt_reason : Sexp.t option;
    index_register : int;
    memory : Memory.t;
    options : Options.t;
    program_counter : int;
    registers : Registers.t;
    _stack : int Stack.t;
    mutable sound_timer : int;
  }
  [@@deriving sexp_of]

  let init ~(options : Options.t) =
    let display =
      match options.disable_graphics with
      | true -> Display.Testing.init_no_graphics ()
      | false -> Display.init ()
    in
    {
      delay_timer = 0;
      display;
      halt_reason = None;
      index_register = 0;
      memory = Memory.init ();
      options;
      program_counter = Memory.Constants.program_start_location;
      registers = Registers.init ();
      _stack = Stack.create ();
      sound_timer = 0;
    }

  let load_program t ~program_file = Memory.load_program t.memory ~program_file
  let display t = t.display
end

let handle_opcode' (state : State.t) (opcode : Opcode.t) =
  match opcode with
  | Add_to_index_register { index } ->
      let to_add = Registers.read_exn state.registers ~index in
      { state with index_register = state.index_register + to_add }
  | Add_to_register { index; to_add } ->
      let old_value = Registers.read_exn state.registers ~index in
      let to_add =
        match to_add with
        | Direct value -> value
        | Register index -> Registers.read_exn state.registers ~index
      in
      let new_value = old_value + to_add in
      let () = Registers.write_exn state.registers ~index new_value in
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
      let () = Registers.write_exn state.registers ~index:x_index new_value in
      state
  | Clear_screen ->
      let display = Display.clear state.display in
      { state with display }
  | Draw { x_index; y_index; num_bytes } ->
      (* N.B. phall (2025-01-27): [x] coordinates are modulo'd _only_ at the beginning.
      The first coordinate always appears on screen, but future pixels that exceed the
      display boundary do not wrap. *)
      let initial_x =
        Registers.read_exn state.registers ~index:x_index
        % Display.Constants.pixel_width
      in
      let initial_y = Registers.read_exn state.registers ~index:y_index in
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
  | Halt ->
      let halt_reason = [%message "Decoded empty opcode (0x0000)!"] |> Some in
      { state with halt_reason }
  | Jump { new_program_counter; with_offset } -> (
      let new_program_counter =
        match with_offset with
        | false -> new_program_counter
        | true ->
            let index =
              match state.options.jump_with_offset with
              | `XNN -> new_program_counter lsr 8
              | `NNN -> 0
            in

            let from_register = Registers.read_exn state.registers ~index in
            new_program_counter + from_register
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
  | Set_index_register { value } -> { state with index_register = value }
  | Set_register { index; to_ } ->
      let value =
        match to_ with
        | Timer Delay -> state.delay_timer
        | Timer Sound -> state.sound_timer
        | Non_timer (Direct value) -> value
        | Non_timer (Register index) ->
            Registers.read_exn state.registers ~index
      in
      let () = Registers.write_exn state.registers ~index value in
      state
  | _ -> raise_s [%message "unimplemented"]

let handle_opcode state ~opcode:raw_opcode =
  let opcode = Opcode.decode_exn raw_opcode in
  handle_opcode' state opcode

let fetch (state : State.t) =
  let first_half = Memory.read state.memory ~loc:state.program_counter in
  let second_half = Memory.read state.memory ~loc:(state.program_counter + 1) in
  (first_half lsl 8) lor second_half

let run ~options ~program_file =
  let state = State.init ~options in
  let%map () = State.load_program state ~program_file in
  (* main fetch, decode, execute loop *)
  let rec loop (state : State.t) =
    match state.halt_reason with
    | Some halt_reason ->
        print_s [%message "HALTING" (halt_reason : Sexp.t)];
        state
    | None ->
        let opcode = fetch state in
        { state with program_counter = state.program_counter + 2 }
        |> handle_opcode ~opcode |> loop
  in
  (* timer loop *)
  Clock_ns.every
    (Time_ns.Span.of_sec (1. /. 60.))
    (fun () ->
      state.delay_timer <- Int.max 0 (state.delay_timer - 1);
      state.sound_timer <- Int.max 0 (state.sound_timer - 1));
  loop state

module Testing = struct
  module Constants = struct
    include Constants

    let bytes_per_char = 5
    let num_hex_chars = 16
  end

  let display_font_opcodes =
    let start_x = 0 in
    let start_y = 0 in
    let display_hexchar_width =
      Display.Constants.pixel_width / Constants.bits_in_byte
    in
    List.init Constants.num_hex_chars ~f:Fn.id
    |> List.concat_map ~f:(fun i ->
           let font_location = i * Constants.bytes_per_char in
           let x = start_x + (i * Constants.bits_in_byte) + 1 in
           let y =
             start_y
             + (i / display_hexchar_width * (Constants.bytes_per_char + 1))
           in
           [
             Opcode.Set_index_register { value = font_location };
             Opcode.Set_register { index = 0; to_ = Non_timer (Direct x) };
             Opcode.Set_register { index = 1; to_ = Non_timer (Direct y) };
             Opcode.Draw
               {
                 x_index = 0;
                 y_index = 1;
                 num_bytes = Constants.bytes_per_char;
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
    let%bind state =
      run ~options:Options.default_for_testing ~program_file:tmp_filename
    in
    let%bind () = Unix.remove tmp_filename in
    return state

  let run_test ~how opcodes =
    let%map (state : State.t) =
      match how with
      | `manual_step -> manually_step_opcodes opcodes
      | `load_and_run -> load_and_run_emulator opcodes
    in
    Display.freeze state.display

  let display_font ~how = run_test ~how display_font_opcodes
  let run = run ~options:Options.default_for_testing
end
