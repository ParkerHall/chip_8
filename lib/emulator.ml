open! Core
open! Async

module Constants = struct
  let bits_in_byte = 8
end

module State = struct
  type t = {
    display : Display.t;
    index_register : int;
    memory : Memory.t;
    program_counter : int;
    registers : Registers.t;
    _stack : int Stack.t;
    is_halted : bool;
  }

  let init () =
    {
      display = Display.init ();
      index_register = 0;
      memory = Memory.init ();
      program_counter = Memory.Constants.program_start_location;
      registers = Registers.init ();
      _stack = Stack.create ();
      is_halted = false;
    }

  let load_program t ~program_file = Memory.load_program t.memory ~program_file
end

let handle_opcode' (state : State.t) (opcode : Opcode.t) =
  match opcode with
  | Halt -> { state with is_halted = true }
  | Clear_screen ->
      let display = Display.clear state.display in
      { state with display }
  | Jump { new_program_counter } ->
      { state with program_counter = new_program_counter }
  | Set_register { index; value } ->
      let () = Registers.write_exn state.registers ~index value in
      state
  | Add_to_register { index; value } ->
      let old_value = Registers.read_exn state.registers ~index in
      let new_value = old_value + value in
      let () = Registers.write_exn state.registers ~index new_value in
      state
  | Set_index_register { value } -> { state with index_register = value }
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

let handle_opcode state ~opcode:raw_opcode =
  let opcode = Opcode.decode_exn raw_opcode in
  handle_opcode' state opcode

let fetch (state : State.t) =
  let first_half = Memory.read state.memory ~loc:state.program_counter in
  let second_half = Memory.read state.memory ~loc:(state.program_counter + 1) in
  (first_half lsl 8) lor second_half

let run ~program_file =
  let state = State.init () in
  let%map () = State.load_program state ~program_file in
  (* main fetch, decode, execute loop *)
  let rec loop (state : State.t) =
    match state.is_halted with
    | true ->
        print_s [%message "HALTING PROGRAM AFTER READING EMPTY OPCODE"];
        state
    | false ->
        let opcode = fetch state in
        { state with program_counter = state.program_counter + 2 }
        |> handle_opcode ~opcode |> loop
  in
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
             Opcode.Set_register { index = 0; value = x };
             Opcode.Set_register { index = 1; value = y };
             Opcode.Draw
               {
                 x_index = 0;
                 y_index = 1;
                 num_bytes = Constants.bytes_per_char;
               };
           ])

  let manually_step_opcodes opcodes =
    List.fold opcodes ~init:(State.init ()) ~f:(fun state opcode ->
        handle_opcode' state opcode)
    |> return

  let load_and_run_emulator opcodes =
    let tmp_filename = Filename_unix.temp_file "opcodes" "binary" in
    let%bind () =
      Writer.with_file tmp_filename ~f:(fun writer ->
          Deferred.List.iter opcodes ~how:`Sequential ~f:(fun opcode ->
              let binary = Opcode.encode opcode in
              Writer.write_byte writer (binary lsr 8);
              Writer.write_byte writer (binary land 0xFF);
              Deferred.unit))
    in
    let%bind state = run ~program_file:tmp_filename in
    let%bind () = Unix.remove tmp_filename in
    return state

  let run_test ~how opcodes =
    let%map (state : State.t) =
      match how with
      | `manual_step -> manually_step_opcodes opcodes
      | `load_and_run -> load_and_run_emulator opcodes
    in
    Display.Testing.freeze state.display

  let display_font ~how = run_test ~how display_font_opcodes
end
