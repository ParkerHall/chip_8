open! Core
open! Import

type t = Up | Right | Down | Left [@@deriving enumerate, variants]

val encode : t -> int
val reverse : t -> t

module Movement : sig
  type t = { sign : [ `Pos | `Neg ]; magnitude : int }
end

val movement : t -> Movement.t
