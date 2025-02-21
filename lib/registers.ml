open! Core

module Constants = struct
  let num_registers = 16
  let flag_register_index = 0xF
end

type t = int array [@@deriving sexp_of]

let init () = Array.create ~len:Constants.num_registers 0x0
let read_exn t ~index = t.(index) land 0xFF
let write_exn t ~index value = t.(index) <- value land 0xFF
let set_flag_register t = write_exn t ~index:Constants.flag_register_index 0x1
let unset_flag_register t = write_exn t ~index:Constants.flag_register_index 0x0
