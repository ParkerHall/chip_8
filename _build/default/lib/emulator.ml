open! Core

module State = struct
  type t = {
    display : Display.t;
    index_register : Memory.Location.t;
    memory : Memory.t;
    program_counter : Memory.Location.t;
    registers : Registers.t;
    stack : Memory.Location.t Stack.t;
  }

  let create () =
    {
      display = Display.init ();
      index_register = Memory.Location.of_int_exn 0;
      memory = Memory.init ();
      program_counter = Memory.Location.of_int_exn 0;
      registers = Registers.init ();
      stack = Stack.create ();
    }
end

let handle_opcode (state : State.t) uint16 =
  let opcode = Opcode.decode_exn uint16 in
  match opcode with
  | Clear_screen ->
      let display = Display.clear state.display in
      { state with display }
  | Jump program_counter -> { state with program_counter }
  | Set_register { index; value } ->
      let () = Registers.write_exn state.registers index value in
      state
  | Add_to_register { index; value } ->
      let old_value = Registers.read_exn state.registers index in
      let new_value = Registers.Value.add old_value value in
      let () = Registers.write_exn state.registers index new_value in
      state
  | Set_index_register index_register -> { state with index_register }
  | Draw _ -> (* TODO: Implement *) state

let run_test () =
  let memory = Memory.init () in
  List.init 4096 ~f:Fn.id
  |> List.iter ~f:(fun loc ->
         let value = loc land 0xFF |> Unsigned.UInt8.of_int in
         let loc = Memory.Location.of_int_exn loc in
         Memory.write memory ~loc value);
  print_endline (Memory.to_string_hum memory)
