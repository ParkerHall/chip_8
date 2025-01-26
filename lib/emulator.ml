open! Core

module Constants = struct
  let bytes_in_memory = 4096
end

let run_test () =
  let memory = Memory.create () in
  List.init 4096 ~f:Fn.id
  |> List.iter ~f:(fun loc ->
         let value = loc land 0xFF |> Unsigned.UInt8.of_int in
         Memory.write memory ~loc value);
  print_endline (Memory.to_string_hum memory)
