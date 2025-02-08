open! Core

module Constants : sig
  val pixel_width : int
end

module Location : sig
  type t

  val create : x:int -> y:int -> t
end

type t

val init : unit -> t
val flip : t -> Location.t -> t * [ `Set | `Unset | `Out_of_bounds ]
val is_set : t -> Location.t -> bool
val clear : t -> t

(* freezes display and blocks on user input *)
val freeze : t -> unit

module Testing : sig
  (* disables use of X11 [Graphics] module *)
  val init_no_graphics : unit -> t
  val dump_to_stdout : t -> unit
end
