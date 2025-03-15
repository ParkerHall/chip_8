open! Core

type t = private { x : int; y : int }

val create : x:int -> y:int -> t

include Comparable.S with type t := t
