open! Core
open! Import

type t =
  | Finalized of Opcode.t
  | Jump of { relative_to_self : int }
  | Set_index_register_to_draw_region of { offset : int }
  | Set_index_register_to_state_region of { offset : int }
  | Subroutine_body of Opcode.t list
      (** [Subroutine_body] is expected to contain neither [Subroutine_start]
          nor [Subroutine_end] opcodes *)
[@@deriving variants]

val finalize_all : scratch_bytes_for_draw:int -> t list -> Opcode.t list
