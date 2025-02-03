open! Core
open! Async

module Testing : sig
  val display_font : how:[ `manual_step | `load_and_run ] -> unit Deferred.t
end
