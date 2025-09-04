open! Core
open! Async
open! Import

module Constants = struct
  let num_registers = Constants.num_registers
  let flag_register_index = 0xF
end

type t = int array [@@deriving sexp_of]

let init () = Array.create ~len:Constants.num_registers 0x0

let read_exn t ~index =
  let value = t.(index) land 0xFF in
  [%log.global.debug "Reading register" (index : int) (value : int)];
  value

let write_exn t ~index value =
  [%log.global.debug "Writing register" (index : int) (value : int)];
  t.(index) <- value land 0xFF

let set_flag_register t = write_exn t ~index:Constants.flag_register_index 0x1
let unset_flag_register t = write_exn t ~index:Constants.flag_register_index 0x0
