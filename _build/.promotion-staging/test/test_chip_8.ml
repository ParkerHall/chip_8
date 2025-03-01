open! Core
open! Async
open Chip_8

let%expect_test "dump IBM logo" =
  let%map state = Emulator.Testing.run ~program_file:"./ibm-logo.ch8" in
  Emulator.State.display state |> Display.Testing.dump_to_stdout;
  [%expect
    {|
    (opcode Clear_screen)
    (opcode (Set_index_register (value 554)))
    (opcode (Set_register (index 0) (to_ (Non_timer (Direct 12)))))
    (opcode (Set_register (index 1) (to_ (Non_timer (Direct 8)))))
    (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
    (opcode (Add_to_register (index 0) (to_add (Direct 9))))
    (opcode (Set_index_register (value 569)))
    (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
    (opcode (Set_index_register (value 584)))
    (opcode (Add_to_register (index 0) (to_add (Direct 8))))
    (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
    (opcode (Add_to_register (index 0) (to_add (Direct 4))))
    (opcode (Set_index_register (value 599)))
    (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
    (opcode (Add_to_register (index 0) (to_add (Direct 8))))
    (opcode (Set_index_register (value 614)))
    (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
    (opcode (Add_to_register (index 0) (to_add (Direct 8))))
    (opcode (Set_index_register (value 629)))
    (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
    (opcode (Jump (new_program_counter_base 552) (with_offset false)))
    (HALTING
     (halt_reason
      ("Encountered infinite JUMP loop!" (opcode_program_counter 552)
       (opcode (Jump (new_program_counter_base 552) (with_offset false))))))








                ######## #########   #####         #####

                ######## ########### ######       ######

                  ####     ###   ###   #####     #####

                  ####     #######     ####### #######

                  ####     #######     ### ####### ###

                  ####     ###   ###   ###  #####  ###

                ######## ########### #####   ###   #####

                ######## #########   #####    #    #####
    |}]
