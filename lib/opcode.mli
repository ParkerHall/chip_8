open! Core

type t =
  | Clear_screen
  | Jump of Memory.Location.t
  | Set_register of { index : Registers.Index.t; value : Registers.Value.t }
  | Add_to_register of { index : Registers.Index.t; value : Registers.Value.t }
  | Set_index_register of Memory.Location.t
  | Draw of { x : Registers.Index.t; y : Registers.Index.t; height : int }

val decode_exn : Unsigned.UInt16.t -> t
