open Core
open Async
open Chip_8

module How_to_run_test = struct
  type t = [ `manual_step | `load_and_run ]

  let flag : t Command.Param.t =
    let open Command.Param in
    choose_one
      [
        flag "manual-step" no_arg
          ~doc:" if specified, manually step through already-decoded opcodes"
        >>| Fn.flip Option.some_if `manual_step;
        flag "load-and-run" no_arg
          ~doc:" if specified, load opcodes from program file and run"
        >>| Fn.flip Option.some_if `load_and_run;
      ]
      ~if_nothing_chosen:Raise
end

let testing_cmd =
  Command.async ~summary:"test chip-8 emulator"
    (let%map_open.Command test_name =
       flag "test-name" (required string) ~doc:"STRING name of test to run"
     and how = How_to_run_test.flag in
     fun () ->
       let test =
         match String.lowercase test_name with
         | "display-font" -> Emulator.Testing.display_font
         | _ -> raise_s [%message "Unknown test name" (test_name : string)]
       in
       test ~how)

let command = Command.group ~summary:"chip-8 emulator" [ ("test", testing_cmd) ]
let () = Command_unix.run command
