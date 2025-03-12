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
    ((opcode Clear_screen) ("state.State.program_counter - 2" 512))
    ((opcode (Set_index_register (value 554)))
     ("state.State.program_counter - 2" 514))
    ((opcode (Set_register (index 0) (to_ (Non_timer (Direct 12)))))
     ("state.State.program_counter - 2" 516))
    ((opcode (Set_register (index 1) (to_ (Non_timer (Direct 8)))))
     ("state.State.program_counter - 2" 518))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
     ("state.State.program_counter - 2" 520))
    ((opcode (Add_to_register (index 0) (to_add (Direct 9))))
     ("state.State.program_counter - 2" 522))
    ((opcode (Set_index_register (value 569)))
     ("state.State.program_counter - 2" 524))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
     ("state.State.program_counter - 2" 526))
    ((opcode (Set_index_register (value 584)))
     ("state.State.program_counter - 2" 528))
    ((opcode (Add_to_register (index 0) (to_add (Direct 8))))
     ("state.State.program_counter - 2" 530))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
     ("state.State.program_counter - 2" 532))
    ((opcode (Add_to_register (index 0) (to_add (Direct 4))))
     ("state.State.program_counter - 2" 534))
    ((opcode (Set_index_register (value 599)))
     ("state.State.program_counter - 2" 536))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
     ("state.State.program_counter - 2" 538))
    ((opcode (Add_to_register (index 0) (to_add (Direct 8))))
     ("state.State.program_counter - 2" 540))
    ((opcode (Set_index_register (value 614)))
     ("state.State.program_counter - 2" 542))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
     ("state.State.program_counter - 2" 544))
    ((opcode (Add_to_register (index 0) (to_add (Direct 8))))
     ("state.State.program_counter - 2" 546))
    ((opcode (Set_index_register (value 629)))
     ("state.State.program_counter - 2" 548))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 15)))
     ("state.State.program_counter - 2" 550))
    ((opcode (Jump (new_program_counter_base 552) (with_offset false)))
     ("state.State.program_counter - 2" 552))
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
    ((opcode Clear_screen) ("state.State.program_counter - 2" 512))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 514))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 516))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 238)))))
     ("state.State.program_counter - 2" 518))
    ((opcode
      (Skip_if_register (left_index 5) (right (Direct 238)) (skip_if Equal)))
     ("state.State.program_counter - 2" 520))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 524))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 2)))))
     ("state.State.program_counter - 2" 526))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 238)))))
     ("state.State.program_counter - 2" 528))
    ((opcode (Set_register (index 6) (to_ (Non_timer (Direct 238)))))
     ("state.State.program_counter - 2" 530))
    ((opcode
      (Skip_if_register (left_index 5) (right (Register 6)) (skip_if Equal)))
     ("state.State.program_counter - 2" 532))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 536))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 3)))))
     ("state.State.program_counter - 2" 538))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 238)))))
     ("state.State.program_counter - 2" 540))
    ((opcode
      (Skip_if_register (left_index 5) (right (Direct 253)) (skip_if Not_equal)))
     ("state.State.program_counter - 2" 542))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 546))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 4)))))
     ("state.State.program_counter - 2" 548))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 238)))))
     ("state.State.program_counter - 2" 550))
    ((opcode (Add_to_register (index 5) (to_add (Direct 1))))
     ("state.State.program_counter - 2" 552))
    ((opcode
      (Skip_if_register (left_index 5) (right (Direct 239)) (skip_if Equal)))
     ("state.State.program_counter - 2" 554))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 558))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 5)))))
     ("state.State.program_counter - 2" 560))
    ((opcode (Set_register (index 15) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 562))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 238)))))
     ("state.State.program_counter - 2" 564))
    ((opcode (Set_register (index 6) (to_ (Non_timer (Direct 239)))))
     ("state.State.program_counter - 2" 566))
    ((opcode (Subtract (x_index 5) (y_index 6) (set_x_to x_minus_y)))
     ("state.State.program_counter - 2" 568))
    ((opcode
      (Skip_if_register (left_index 15) (right (Direct 0)) (skip_if Equal)))
     ("state.State.program_counter - 2" 570))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 574))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 6)))))
     ("state.State.program_counter - 2" 576))
    ((opcode (Set_register (index 15) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 578))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 239)))))
     ("state.State.program_counter - 2" 580))
    ((opcode (Set_register (index 6) (to_ (Non_timer (Direct 238)))))
     ("state.State.program_counter - 2" 582))
    ((opcode (Subtract (x_index 5) (y_index 6) (set_x_to x_minus_y)))
     ("state.State.program_counter - 2" 584))
    ((opcode
      (Skip_if_register (left_index 15) (right (Direct 1)) (skip_if Equal)))
     ("state.State.program_counter - 2" 586))
    ((opcode (Set_register (index 15) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 590))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 592))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 7)))))
     ("state.State.program_counter - 2" 594))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 238)))))
     ("state.State.program_counter - 2" 596))
    ((opcode (Set_register (index 6) (to_ (Non_timer (Direct 239)))))
     ("state.State.program_counter - 2" 598))
    ((opcode (Subtract (x_index 5) (y_index 6) (set_x_to y_minus_x)))
     ("state.State.program_counter - 2" 600))
    ((opcode
      (Skip_if_register (left_index 15) (right (Direct 1)) (skip_if Equal)))
     ("state.State.program_counter - 2" 602))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 606))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 8)))))
     ("state.State.program_counter - 2" 608))
    ((opcode (Set_register (index 15) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 610))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 239)))))
     ("state.State.program_counter - 2" 612))
    ((opcode (Set_register (index 6) (to_ (Non_timer (Direct 238)))))
     ("state.State.program_counter - 2" 614))
    ((opcode (Subtract (x_index 5) (y_index 6) (set_x_to y_minus_x)))
     ("state.State.program_counter - 2" 616))
    ((opcode
      (Skip_if_register (left_index 15) (right (Direct 0)) (skip_if Equal)))
     ("state.State.program_counter - 2" 618))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 622))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 9)))))
     ("state.State.program_counter - 2" 624))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 240)))))
     ("state.State.program_counter - 2" 626))
    ((opcode (Set_register (index 6) (to_ (Non_timer (Direct 15)))))
     ("state.State.program_counter - 2" 628))
    ((opcode (Binary_operation (x_index 5) (y_index 6) (operation OR)))
     ("state.State.program_counter - 2" 630))
    ((opcode
      (Skip_if_register (left_index 5) (right (Direct 255)) (skip_if Equal)))
     ("state.State.program_counter - 2" 632))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 636))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 638))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 240)))))
     ("state.State.program_counter - 2" 640))
    ((opcode (Set_register (index 6) (to_ (Non_timer (Direct 15)))))
     ("state.State.program_counter - 2" 642))
    ((opcode (Binary_operation (x_index 5) (y_index 6) (operation AND)))
     ("state.State.program_counter - 2" 644))
    ((opcode
      (Skip_if_register (left_index 5) (right (Direct 0)) (skip_if Equal)))
     ("state.State.program_counter - 2" 646))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 650))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 652))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 240)))))
     ("state.State.program_counter - 2" 654))
    ((opcode (Set_register (index 6) (to_ (Non_timer (Direct 15)))))
     ("state.State.program_counter - 2" 656))
    ((opcode (Binary_operation (x_index 5) (y_index 6) (operation XOR)))
     ("state.State.program_counter - 2" 658))
    ((opcode
      (Skip_if_register (left_index 5) (right (Direct 255)) (skip_if Equal)))
     ("state.State.program_counter - 2" 660))
    ((opcode (Set_register (index 15) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 664))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 666))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 2)))))
     ("state.State.program_counter - 2" 668))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 129)))))
     ("state.State.program_counter - 2" 670))
    ((opcode (Shift (x_index 5) (y_index 0) (direction left)))
     ("state.State.program_counter - 2" 672))
    ((opcode
      (Skip_if_register (left_index 15) (right (Direct 1)) (skip_if Equal)))
     ("state.State.program_counter - 2" 674))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 678))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 3)))))
     ("state.State.program_counter - 2" 680))
    ((opcode (Set_register (index 15) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 682))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 71)))))
     ("state.State.program_counter - 2" 684))
    ((opcode (Shift (x_index 5) (y_index 0) (direction left)))
     ("state.State.program_counter - 2" 686))
    ((opcode
      (Skip_if_register (left_index 15) (right (Direct 0)) (skip_if Equal)))
     ("state.State.program_counter - 2" 688))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 692))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 4)))))
     ("state.State.program_counter - 2" 694))
    ((opcode (Set_register (index 15) (to_ (Non_timer (Direct 0)))))
     ("state.State.program_counter - 2" 696))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 698))
    ((opcode (Shift (x_index 5) (y_index 0) (direction right)))
     ("state.State.program_counter - 2" 700))
    ((opcode
      (Skip_if_register (left_index 15) (right (Direct 1)) (skip_if Equal)))
     ("state.State.program_counter - 2" 702))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 706))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 5)))))
     ("state.State.program_counter - 2" 708))
    ((opcode (Set_register (index 15) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 710))
    ((opcode (Set_register (index 5) (to_ (Non_timer (Direct 2)))))
     ("state.State.program_counter - 2" 712))
    ((opcode (Shift (x_index 5) (y_index 0) (direction right)))
     ("state.State.program_counter - 2" 714))
    ((opcode
      (Skip_if_register (left_index 15) (right (Direct 0)) (skip_if Equal)))
     ("state.State.program_counter - 2" 716))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 720))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 6)))))
     ("state.State.program_counter - 2" 722))
    ((opcode (Set_register (index 0) (to_ (Non_timer (Direct 21)))))
     ("state.State.program_counter - 2" 724))
    ((opcode (Set_register (index 1) (to_ (Non_timer (Direct 120)))))
     ("state.State.program_counter - 2" 726))
    ((opcode (Set_index_register (value 976)))
     ("state.State.program_counter - 2" 728))
    ((opcode (Store (up_to_index 1))) ("state.State.program_counter - 2" 730))
    ((opcode (Load (up_to_index 1))) ("state.State.program_counter - 2" 732))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 21)) (skip_if Equal)))
     ("state.State.program_counter - 2" 734))
    ((opcode
      (Skip_if_register (left_index 1) (right (Direct 120)) (skip_if Equal)))
     ("state.State.program_counter - 2" 738))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 742))
    ((opcode (Set_register (index 4) (to_ (Non_timer (Direct 7)))))
     ("state.State.program_counter - 2" 744))
    ((opcode (Set_register (index 0) (to_ (Non_timer (Direct 138)))))
     ("state.State.program_counter - 2" 746))
    ((opcode (Set_index_register (value 976)))
     ("state.State.program_counter - 2" 748))
    ((opcode (Decimal_conversion (index 0)))
     ("state.State.program_counter - 2" 750))
    ((opcode (Set_index_register (value 976)))
     ("state.State.program_counter - 2" 752))
    ((opcode (Load (up_to_index 0))) ("state.State.program_counter - 2" 754))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 1)) (skip_if Equal)))
     ("state.State.program_counter - 2" 756))
    ((opcode (Set_register (index 0) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 760))
    ((opcode (Add_to_index_register (index 0)))
     ("state.State.program_counter - 2" 762))
    ((opcode (Load (up_to_index 0))) ("state.State.program_counter - 2" 764))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 3)) (skip_if Equal)))
     ("state.State.program_counter - 2" 766))
    ((opcode (Set_register (index 0) (to_ (Non_timer (Direct 1)))))
     ("state.State.program_counter - 2" 770))
    ((opcode (Add_to_index_register (index 0)))
     ("state.State.program_counter - 2" 772))
    ((opcode (Load (up_to_index 0))) ("state.State.program_counter - 2" 774))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 8)) (skip_if Equal)))
     ("state.State.program_counter - 2" 776))
    ((opcode (Jump (new_program_counter_base 818) (with_offset false)))
     ("state.State.program_counter - 2" 780))
    ((opcode (Set_index_register (value 856)))
     ("state.State.program_counter - 2" 818))
    ((opcode (Set_register (index 0) (to_ (Non_timer (Direct 21)))))
     ("state.State.program_counter - 2" 820))
    ((opcode (Set_register (index 1) (to_ (Non_timer (Direct 11)))))
     ("state.State.program_counter - 2" 822))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 8)))))
     ("state.State.program_counter - 2" 824))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 826))
    ((opcode (Add_to_register (index 0) (to_add (Direct 8))))
     ("state.State.program_counter - 2" 828))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 830))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 45)) (skip_if Equal)))
     ("state.State.program_counter - 2" 832))
    ((opcode (Jump (new_program_counter_base 826) (with_offset false)))
     ("state.State.program_counter - 2" 834))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 826))
    ((opcode (Add_to_register (index 0) (to_add (Direct 8))))
     ("state.State.program_counter - 2" 828))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 830))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 45)) (skip_if Equal)))
     ("state.State.program_counter - 2" 832))
    ((opcode (Jump (new_program_counter_base 826) (with_offset false)))
     ("state.State.program_counter - 2" 834))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 826))
    ((opcode (Add_to_register (index 0) (to_add (Direct 8))))
     ("state.State.program_counter - 2" 828))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 830))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 45)) (skip_if Equal)))
     ("state.State.program_counter - 2" 832))
    ((opcode (Set_index_register (value 880)))
     ("state.State.program_counter - 2" 836))
    ((opcode (Set_register (index 0) (to_ (Non_timer (Direct 2)))))
     ("state.State.program_counter - 2" 838))
    ((opcode (Set_register (index 1) (to_ (Non_timer (Direct 24)))))
     ("state.State.program_counter - 2" 840))
    ((opcode (Set_register (index 3) (to_ (Non_timer (Direct 8)))))
     ("state.State.program_counter - 2" 842))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 844) (with_offset false)))
     ("state.State.program_counter - 2" 852))
    ((opcode (Draw (x_index 0) (y_index 1) (num_bytes 8)))
     ("state.State.program_counter - 2" 844))
    ((opcode (Add_to_register (index 0) (to_add (Direct 5))))
     ("state.State.program_counter - 2" 846))
    ((opcode (Add_to_index_register (index 3)))
     ("state.State.program_counter - 2" 848))
    ((opcode
      (Skip_if_register (left_index 0) (right (Direct 62)) (skip_if Equal)))
     ("state.State.program_counter - 2" 850))
    ((opcode (Jump (new_program_counter_base 782) (with_offset false)))
     ("state.State.program_counter - 2" 854))
    ((opcode (Jump (new_program_counter_base 782) (with_offset false)))
     ("state.State.program_counter - 2" 782))
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
