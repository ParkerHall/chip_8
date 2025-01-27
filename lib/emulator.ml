open! Core

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
    stack : int Stack.t;
  }

  let create () =
    {
      display = Display.init ();
      index_register = 0;
      memory = Memory.init ();
      program_counter = 0;
      registers = Registers.init ();
      stack = Stack.create ();
    }
end

let handle_opcode' (state : State.t) (opcode : Opcode.t) =
  match opcode with
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
      let initial_x = Registers.read_exn state.registers ~index:x_index in
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
                            Display.flip display ~x ~y
                          in
                          let () =
                            match pixel_status with
                            | `Out_of_bounds -> print_endline "out of bounds"
                            | `Set -> print_endline "set"
                            | `Unset ->
                                print_endline "unset";
                                Registers.set_flag_register state.registers
                          in
                          new_display))
      in
      { state with display }

let handle_opcode state raw_opcode =
  let opcode = Opcode.decode_exn raw_opcode in
  handle_opcode' state opcode

module Testing = struct
  module Constants = struct
    include Constants

    let bytes_per_char = 5
    let num_hex_chars = 16
  end

  let display_font_opcodes =
    let start_x = 0 in
    let start_y = 0 in
    List.init Constants.num_hex_chars ~f:Fn.id
    |> List.concat_map ~f:(fun i ->
           [
             Opcode.Set_index_register { value = i * Constants.bytes_per_char };
             Opcode.Set_register
               { index = 0; value = start_x + (i * Constants.bits_in_byte) };
             Opcode.Set_register { index = 1; value = start_y };
             Opcode.Draw
               {
                 x_index = 0;
                 y_index = 1;
                 num_bytes = Constants.bytes_per_char;
               };
           ])

  let run_test opcodes =
    let state =
      List.fold opcodes ~init:(State.create ()) ~f:(fun state opcode ->
          handle_opcode' state opcode)
    in
    Display.Testing.freeze state.display

  let display_font () = run_test display_font_opcodes
end
