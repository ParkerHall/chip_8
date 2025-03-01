open! Core
open! Async

module Options : sig
  type t = {
    detect_jump_self_loop : bool;
    disable_graphics : bool;
    ignore_y_on_shift : bool;
    increment_index_on_store_or_load : bool;
    jump_with_offset : [ `NNN | `XNN ];
    keypress_frequency : Time_ns.Span.t;
  }

  val default : t
  val flag : t Command.Param.t
end

module State : sig
  type t

  val display : t -> Display.t
end

val run : options:Options.t -> program_file:string -> State.t Deferred.t

module Testing : sig
  (* runs with graphics disabled and JUMP self-loop halting *)
  val run : program_file:string -> State.t Deferred.t
  val display_font : how:[ `manual_step | `load_and_run ] -> unit Deferred.t
end
