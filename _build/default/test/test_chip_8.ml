open! Core
open! Async
open! Chip_8

let%expect_test "dump font" =
  let%map () = Emulator.Testing.display_font ~how:`manual_step in
  [%expect
    {|
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
