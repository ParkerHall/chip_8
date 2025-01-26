open! Core

module Constants = struct
  let num_registers = 16
end

module Value = struct
  let min_val = 0
  let max_val = Unsigned.UInt8.(to_int max_int)

  type t = Unsigned.UInt8.t

  let zero = Unsigned.UInt8.zero
  let add = Unsigned.UInt8.add

  let of_int_exn int =
    match int < min_val || int > max_val with
    | true ->
        raise_s
          [%message
            "Register value out of range"
              (int : int)
              (min_val : int)
              (max_val : int)]
    | false -> Unsigned.UInt8.of_int int
end

module Index = struct
  type t = int

  let of_int_exn int =
    match int < 0 || int > Constants.num_registers - 1 with
    | true ->
        raise_s
          [%message
            "Index out of bounds" (int : int) (Constants.num_registers : int)]
    | false -> int
end

type t = Value.t array

let init () = Array.create ~len:Constants.num_registers Value.zero
let read_exn t index = t.(index)
let write_exn t index value = t.(index) <- value
