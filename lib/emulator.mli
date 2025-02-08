open! Core
open! Async

module State : sig
  type t

  val display : t -> Display.t
end

val run : program_file:string -> State.t Deferred.t

module Testing : sig
  (* runs with graphics disabled and JUMP self-loop halting *)
  val run : program_file:string -> State.t Deferred.t
  val display_font : how:[ `manual_step | `load_and_run ] -> unit Deferred.t
end
