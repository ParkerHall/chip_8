open! Core
open! Async
open! Chip_8

let%expect_test "dump font" =
  let%map () = Emulator.Testing.display_font ~how:`manual_step in
  [%expect
    {|
    (opcodes
     ((Set_index_register (value 0))
      (Set_register (index 0) (to_ (Non_timer (Direct 1))))
      (Set_register (index 1) (to_ (Non_timer (Direct 0))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5)) (Set_index_register (value 5))
      (Set_register (index 0) (to_ (Non_timer (Direct 9))))
      (Set_register (index 1) (to_ (Non_timer (Direct 0))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 10))
      (Set_register (index 0) (to_ (Non_timer (Direct 17))))
      (Set_register (index 1) (to_ (Non_timer (Direct 0))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 15))
      (Set_register (index 0) (to_ (Non_timer (Direct 25))))
      (Set_register (index 1) (to_ (Non_timer (Direct 0))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 20))
      (Set_register (index 0) (to_ (Non_timer (Direct 33))))
      (Set_register (index 1) (to_ (Non_timer (Direct 0))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 25))
      (Set_register (index 0) (to_ (Non_timer (Direct 41))))
      (Set_register (index 1) (to_ (Non_timer (Direct 0))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 30))
      (Set_register (index 0) (to_ (Non_timer (Direct 49))))
      (Set_register (index 1) (to_ (Non_timer (Direct 0))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 35))
      (Set_register (index 0) (to_ (Non_timer (Direct 57))))
      (Set_register (index 1) (to_ (Non_timer (Direct 0))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 40))
      (Set_register (index 0) (to_ (Non_timer (Direct 65))))
      (Set_register (index 1) (to_ (Non_timer (Direct 6))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 45))
      (Set_register (index 0) (to_ (Non_timer (Direct 73))))
      (Set_register (index 1) (to_ (Non_timer (Direct 6))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 50))
      (Set_register (index 0) (to_ (Non_timer (Direct 81))))
      (Set_register (index 1) (to_ (Non_timer (Direct 6))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 55))
      (Set_register (index 0) (to_ (Non_timer (Direct 89))))
      (Set_register (index 1) (to_ (Non_timer (Direct 6))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 60))
      (Set_register (index 0) (to_ (Non_timer (Direct 97))))
      (Set_register (index 1) (to_ (Non_timer (Direct 6))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 65))
      (Set_register (index 0) (to_ (Non_timer (Direct 105))))
      (Set_register (index 1) (to_ (Non_timer (Direct 6))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 70))
      (Set_register (index 0) (to_ (Non_timer (Direct 113))))
      (Set_register (index 1) (to_ (Non_timer (Direct 6))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))
      (Set_index_register (value 75))
      (Set_register (index 0) (to_ (Non_timer (Direct 121))))
      (Set_register (index 1) (to_ (Non_timer (Direct 6))))
      (Draw (x_index 0) (y_index 1) (num_bytes 5))))
     ####      #     ####    ####    #  #    ####    ####    ####
     #  #     ##        #       #    #  #    #       #          #
     #  #      #     ####    ####    ####    ####    ####      #
     #  #      #     #          #       #       #    #  #     #
     ####     ###    ####    ####       #    ####    ####     #

     ####    ####    ####    ###     ####    ###     ####    ####
     #  #    #  #    #  #    #  #    #       #  #    #       #
     ####    ####    ####    ###     #       #  #    ####    ####
     #  #       #    #  #    #  #    #       #  #    #       #
     ####    ####    #  #    ###     ####    ###     ####    #
    |}]

let%expect_test "dump IBM logo" =
  let%map state = Emulator.Testing.run ~program_file:"./ibm-logo.ch8" in
  Emulator.State.display state |> Display.Testing.dump_to_stdout;
  [%expect
    {|
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

let%expect_test "run BC test" =
  let%map state = Emulator.Testing.run ~program_file:"./bc-test.ch8" in
  Emulator.State.display state |> Display.Testing.dump_to_stdout;
  [%expect {|
    (HALTING
     (halt_reason
      ("Encountered infinite JUMP loop!" (opcode_program_counter 782)
       (opcode (Jump (new_program_counter_base 782) (with_offset false))))))











                         ####     ####   #    #
                         #   #   #    #  ##   #
                         #   #   #    #  # #  #
                         ####    #    #  #  # #
                         #   #   #    #  #   ##
                         #   #   #    #  #    #
                         #   #   #    #  #    #
                         ####     ####   #    #





      ##             ##             #    ###         #
      # #            # #            #    #           #
      # #  # #       # #   ##   ##  ##   #     #     #   ##
      ##   # #       ##   # #  #    #    #    # #   ##  # #   ##
      # #  ###       # #  ##    #   #    #    # #  # #  ##    #
      # #    #       # #  #      #  #    #    # #  # #  #     #
      ##     #       ##    ##  ##    ##  ###   #    ##   ##   # #
           ###
    |}]
