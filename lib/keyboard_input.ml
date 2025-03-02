open! Core
open! Async

module Key = struct
  type t = N1 | N2 | N3 | N4 | Q | W | E | R | A | S | D | F | Z | X | C | V
  [@@deriving equal, sexp_of]

  let mapping =
    [
      ('1', N1);
      ('2', N2);
      ('3', N3);
      ('4', N4);
      ('Q', Q);
      ('W', W);
      ('E', E);
      ('R', R);
      ('A', A);
      ('S', S);
      ('D', D);
      ('F', F);
      ('Z', Z);
      ('X', X);
      ('C', C);
      ('V', V);
    ]
    |> Char.Map.of_alist_exn

  let of_char char = Map.find mapping (Char.uppercase char)

  (* 1 2 3 4 -> 1 2 3 C 
     Q W E R -> 4 5 6 D
     A S D F -> 7 8 9 E
     Z X C V -> A 0 B F *)
  let to_int = function
    | N1 -> 0x1
    | N2 -> 0x2
    | N3 -> 0x3
    | N4 -> 0xC
    | Q -> 0x4
    | W -> 0x5
    | E -> 0x6
    | R -> 0xD
    | A -> 0x7
    | S -> 0x8
    | D -> 0x9
    | F -> 0xE
    | Z -> 0xA
    | X -> 0x0
    | C -> 0xB
    | V -> 0xF
end

module Keypress = struct
  type t = { key : Key.t; cycles_remaining : int }

  let step { key; cycles_remaining } =
    match cycles_remaining > 0 with
    | true -> Some { key; cycles_remaining = cycles_remaining - 1 }
    | false -> None
end

module Options = struct
  type t = { frequency : Time_ns.Span.t; repeat_keypress_for_n_cycles : int }
end

type t = {
  mutable current_keypress : Keypress.t option;
  frequency : Time_ns.Span.t;
  graphics_disabled : bool;
  repeat_keypress_for_n_cycles : int;
}

let init { Options.frequency; repeat_keypress_for_n_cycles } =
  {
    current_keypress = None;
    frequency;
    graphics_disabled = false;
    repeat_keypress_for_n_cycles;
  }

let loop_forever t =
  match t.graphics_disabled with
  | true -> ()
  | false ->
      Clock_ns.every t.frequency (fun () ->
          match Graphics.key_pressed () with
          | true ->
              let keypress =
                let%map.Option key = Graphics.read_key () |> Key.of_char in
                {
                  Keypress.key;
                  cycles_remaining = t.repeat_keypress_for_n_cycles;
                }
              in
              t.current_keypress <- keypress
          | false ->
              let new_keypress =
                let%bind.Option old_keypress = t.current_keypress in
                Keypress.step old_keypress
              in
              t.current_keypress <- new_keypress)

let current_key t =
  let%map.Option { Keypress.key; _ } = t.current_keypress in
  key

module Testing = struct
  (* [current_keypress], [frequency], [repeat_keypress_for_n_cycles] unused for testing *)
  let init_no_keypresses () =
    {
      current_keypress = None;
      frequency = Time_ns.Span.max_value_representable;
      graphics_disabled = true;
      repeat_keypress_for_n_cycles = 0;
    }
end
