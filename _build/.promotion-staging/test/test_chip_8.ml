open! Core
open! Async
open Chip_8

let%expect_test "dump IBM logo" =
  let%map state = Emulator.Testing.run ~program_file:"./ibm-logo.ch8" in
  Emulator.State.display state |> Display.Testing.dump_to_stdout;
  [%expect
    {|
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 0)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 514) (registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode Clear_screen))
    ((n1 2) (n2 2) (n3 10))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 0)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 516) (registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Set_index_register (value 554))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 554)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 518) (registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Set_register (index 0) (to_ (Non_timer (Direct 12))))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 554)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 520) (registers (12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Set_register (index 1) (to_ (Non_timer (Direct 8))))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 554)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 522) (registers (12 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 554)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 524) (registers (12 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Add_to_register (index 0) (to_add (Direct 9)))))
    ((n1 2) (n2 3) (n3 9))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 554)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 526) (registers (21 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Set_index_register (value 569))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 569)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 528) (registers (21 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15))))
    ((n1 2) (n2 4) (n3 8))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 569)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 530) (registers (21 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Set_index_register (value 584))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 584)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 532) (registers (21 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Add_to_register (index 0) (to_add (Direct 8)))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 584)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 534) (registers (29 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 584)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 536) (registers (29 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Add_to_register (index 0) (to_add (Direct 4)))))
    ((n1 2) (n2 5) (n3 7))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 584)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 538) (registers (33 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Set_index_register (value 599))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 599)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 540) (registers (33 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 599)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 542) (registers (33 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Add_to_register (index 0) (to_add (Direct 8)))))
    ((n1 2) (n2 6) (n3 6))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 599)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 544) (registers (41 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Set_index_register (value 614))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 614)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 546) (registers (41 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 614)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 548) (registers (41 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Add_to_register (index 0) (to_add (Direct 8)))))
    ((n1 2) (n2 7) (n3 5))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 614)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 550) (registers (49 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Set_index_register (value 629))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 629)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 552) (registers (49 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Draw (x_index 0) (y_index 1) (num_bytes 15))))
    ((state
      ((delay_timer 0) (display <opaque>) (halt_reason ()) (index_register 629)
       (keyboard_input <opaque>) (memory <opaque>)
       (options
        ((detect_jump_self_loop true) (disable_graphics true)
         (ignore_y_on_shift false) (increment_index_on_store_or_load false)
         (jump_with_offset NNN) (keypress_frequency 16.666667ms)))
       (program_counter 554) (registers (49 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
       (stack ()) (sound_timer 0)))
     (opcode (Jump (new_program_counter_base 552) (with_offset false))))
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
