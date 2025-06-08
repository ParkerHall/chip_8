open! Core
open! Import

(* The state layout is:
  - 1 byte: current snake direction
  - 1 byte: current snake x location
  - 1 byte: current snake y location *)
module Value = struct
  type t = { offset : int; length : int } [@@deriving fields ~getters]

  let direction = { offset = 0; length = 1 }
  let head_x = { offset = 1; length = 1 }
  let head_y = { offset = 2; length = 1 }
end

let layout = Value.[ direction; head_x; head_y ]
let total_bytes = List.sum (module Int) layout ~f:Value.length

let%test "[offset] and [length] are correct" =
  let total_length =
    List.fold layout ~init:0 ~f:(fun expected_offset { Value.offset; length } ->
        assert (expected_offset = offset);
        expected_offset + length)
  in
  total_length = total_bytes

let%expect_test "[total_bytes]" =
  print_s [%message (total_bytes : int)];
  [%expect {| (total_bytes 3) |}]
