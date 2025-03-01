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

type t = { mutable current_key : Key.t option; graphics_disabled : bool }

let init () = { current_key = None; graphics_disabled = false }

let loop_forever t ~frequency =
  match t.graphics_disabled with
  | true -> ()
  | false ->
      Clock_ns.every frequency (fun () ->
          print_s [%message (t.current_key : Key.t option)];
          match Graphics.key_pressed () with
          | true -> t.current_key <- Key.of_char (Graphics.read_key ())
          | false -> ())

let take_key t =
  let current_key = t.current_key in
  t.current_key <- None;
  current_key

module Testing = struct
  let init_no_keypresses () = { current_key = None; graphics_disabled = true }
end
