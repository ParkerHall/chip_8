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
     and how = How_to_run_test.flag
     and log_level =
       flag "log-level"
         (optional Async.Log.Level.arg)
         ~doc:"LOG-LEVEL specify log level (default info)"
     in
     fun () ->
       Option.iter log_level ~f:Async.Log.Global.set_level;
       let test =
         match String.lowercase test_name with
         | "display-font" -> Emulator.Testing.display_font
         | "snake-testing" -> Emulator.Testing.snake_testing
         | _ -> raise_s [%message "Unknown test name" (test_name : string)]
       in
       test ~how)

let run_cmd =
  Command.async_or_error ~summary:"run chip-8 emulator"
    (let%map_open.Command program_file =
       flag "program-file" (required string)
         ~doc:"FILE binary file containing chip-8 program"
     and options = Emulator.Options.flag in
     fun () ->
       let%map (_ : Emulator.State.t) = Emulator.run ~options ~program_file in
       Or_error.error_s
         [%message
           "Loaded program unexpectedly finished" (program_file : string)])

let command =
  Command.group ~summary:"chip-8 emulator"
    [ ("run", run_cmd); ("test", testing_cmd) ]

let () = Command_unix.run command
