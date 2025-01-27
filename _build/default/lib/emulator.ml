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

let handle_opcode (state : State.t) raw_opcode =
  let opcode = Opcode.decode_exn raw_opcode in
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
                            | `Out_of_bounds | `Set -> ()
                            | `Unset ->
                                Registers.set_flag_register state.registers
                          in
                          new_display))
      in
      { state with display }
(*
      List.init 8 ~f:(fun j -> (byte lsr (7 - j)) land 1 = 1)
      |> List.iteri ~f:(fun j pixel ->
        let x = x + j in
        let display = if pixel then Display.set state.display ~x ~y else Display.unset state.display ~x ~y in
        { state with display }) *)

let run_test () =
  let memory = Memory.init () in
  List.init 4096 ~f:Fn.id
  |> List.iter ~f:(fun loc ->
         let value = loc land 0xFF in
         Memory.write memory ~loc value);
  print_endline (Memory.to_string_hum memory)
