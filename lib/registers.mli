open! Core

module Value : sig
  type t

  val add : t -> t -> t
  val of_int_exn : int -> t
end

module Index : sig
  type t

  val of_int_exn : int -> t
end

type t

val init : unit -> t

(* [read_exn t ~index ] raises if [i < 0] or [i > NUM REGISTERS] *)
val read_exn : t -> Index.t -> Value.t

(* [write_exn t ~index ] raises if [i < 0] or [i > NUM REGISTERS] *)
val write_exn : t -> Index.t -> Value.t -> unit
