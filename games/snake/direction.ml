open! Core
open! Import

type t = Up | Right | Down | Left [@@deriving enumerate, sexp_of, variants]

let encode = Variants.to_rank
let reverse = function Up -> Down | Right -> Left | Down -> Up | Left -> Right

let%expect_test "[encode]" =
  List.iter all ~f:(fun t -> print_s [%message (t : t) (encode t : int)]);
  [%expect
    {|
    ((t Up) ("encode t" 0))
    ((t Right) ("encode t" 1))
    ((t Down) ("encode t" 2))
    ((t Left) ("encode t" 3))
    |}]

module Movement = struct
  type t = { sign : [ `Pos | `Neg ]; magnitude : int } [@@deriving sexp_of]
end

let movement : t -> Movement.t = function
  | Up -> { sign = `Neg; magnitude = Constants.snakes_per_row }
  | Right -> { sign = `Pos; magnitude = 1 }
  | Down -> { sign = `Pos; magnitude = Constants.snakes_per_row }
  | Left -> { sign = `Neg; magnitude = 1 }
