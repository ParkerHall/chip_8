open! Core
open! Import

module Allowed_in_subroutine : sig
  type t =
    | Finalized of Opcode.t
    | Jump of { relative_to_self : int }
    | Set_index_register_to_draw_region of { offset : int }
    | Set_index_register_to_state_region of { offset : int }
end

type t =
  | Finalized of Opcode.t
  | Jump of { relative_to_self : int }
  | Set_index_register_to_draw_region of { offset : int }
  | Set_index_register_to_state_region of { offset : int }
  | Subroutine_body of Allowed_in_subroutine.t list
      (** [Subroutine_body] is expected to contain neither [Subroutine_start]
          nor [Subroutine_end] opcodes *)
[@@deriving variants]

(* raises if [Subroutine_body] contains a [Subroutine_start]/[Subroutine_end]
    opcode *)
val finalize_all_exn : scratch_bytes_for_draw:int -> t list -> Opcode.t list
