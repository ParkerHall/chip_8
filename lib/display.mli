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

module Testing : sig
  val freeze : t -> unit
end
