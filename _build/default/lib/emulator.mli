open! Core
open! Async

module State : sig
  type t
end

val run : program_file:string -> State.t Deferred.t

module Testing : sig
  val display_font : how:[ `manual_step | `load_and_run ] -> unit Deferred.t
end
