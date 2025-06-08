open! Core
open! Import

module Allowed_in_subroutine = struct
  type t =
    | Finalized of Opcode.t
    | Jump of { relative_to_self : int }
    | Set_index_register_to_draw_region of { offset : int }
    | Set_index_register_to_state_region of { offset : int }
  [@@deriving sexp_of, variants]
end

type t =
  | Finalized of Opcode.t
  | Jump of { relative_to_self : int }
  | Set_index_register_to_draw_region of { offset : int }
  | Set_index_register_to_state_region of { offset : int }
  | Subroutine_body of Allowed_in_subroutine.t list
[@@deriving sexp_of, variants]

module Context = struct
  type t = {
    main_body : Allowed_in_subroutine.t list;
    subroutines : Allowed_in_subroutine.t list;
  }

  let empty = { main_body = []; subroutines = [] }

  (* this is horribly inefficient, but the number of opcodes in any program
     I write should be sufficiently small to make this not matter! *)
  let append t ~main_body ~subroutine =
    {
      main_body = t.main_body @ [ main_body ];
      subroutines = t.subroutines @ subroutine;
    }

  let to_opcodes { main_body; subroutines } = main_body @ subroutines
end

let assert_no_nested_subroutines ts =
  List.iter ts ~f:(function
    | Subroutine_body opcodes as subroutine ->
        List.iter opcodes ~f:(function
          | Finalized (Opcode.Subroutine_start _ | Opcode.Subroutine_end) ->
              raise_s
                [%message
                  "Nested subroutines are not supported"
                    (subroutine : t)
                    (ts : t list)]
          | Finalized _ | Jump _ | Set_index_register_to_draw_region _
          | Set_index_register_to_state_region _ ->
              ())
    | Finalized _ | Jump _ | Set_index_register_to_draw_region _
    | Set_index_register_to_state_region _ ->
        ())

(* Every [Subroutine_body opcodes] is Allowed_in_subroutine mapped into a [Subroutine_start] in the main body
   and [opcodes; Subroutine_end; Halt] in the subroutine memory region *)
let flatten_subroutines_exn
    ?(start_memory_location = Constants.program_start_memory_location) ts =
  (* adding a [Halt] effectively spaces the main body with the subroutine region *)
  let () = assert_no_nested_subroutines ts in
  let ts = ts @ [ Finalized Opcode.Halt ] in
  (* note that this math only works because each [t] corresponds to exactly 1 finalized [Opcode.t] *)
  let initial_num_opcodes = List.length ts in
  let next_subroutine_start (context : Context.t) =
    let opcodes_til_subroutine =
      initial_num_opcodes + List.length context.subroutines
    in
    start_memory_location + (opcodes_til_subroutine * 2)
  in
  List.fold ts ~init:Context.empty ~f:(fun context t ->
      let (main_body : Allowed_in_subroutine.t), subroutine =
        match t with
        | Finalized opcode -> (Finalized opcode, [])
        | Jump { relative_to_self } -> (Jump { relative_to_self }, [])
        | Set_index_register_to_draw_region { offset } ->
            (Set_index_register_to_draw_region { offset }, [])
        | Set_index_register_to_state_region { offset } ->
            (Set_index_register_to_state_region { offset }, [])
        | Subroutine_body opcodes ->
            let main_body =
              let next_subroutine_start = next_subroutine_start context in
              Opcode.Subroutine_start
                { memory_location = next_subroutine_start }
              |> Allowed_in_subroutine.Finalized
            in
            let suffix =
              Opcode.[ Subroutine_end; Halt ]
              |> List.map ~f:Allowed_in_subroutine.finalized
            in
            let subroutine = opcodes @ suffix in
            (main_body, subroutine)
      in
      Context.append context ~main_body ~subroutine)
  |> Context.to_opcodes

let%expect_test "[flatten_subroutines_exn] - standard" =
  let subroutine_body opcodes =
    let allowed_in_subroutine =
      List.map opcodes ~f:Allowed_in_subroutine.finalized
    in
    Subroutine_body allowed_in_subroutine
  in
  let opcodes =
    [
      Set_index_register_to_draw_region { offset = 0 };
      Jump { relative_to_self = 1 };
      subroutine_body
        Opcode.
          [
            Add_to_register { index = 2; to_add = Direct 3 };
            Set_index_register { value = 4 };
            Draw { x_index = 5; y_index = 6; num_bytes = 7 };
          ];
      Finalized (Opcode.Get_key { index = 8 });
      subroutine_body Opcode.[ Get_key { index = 9 } ];
      Finalized Opcode.Clear_screen;
      Finalized (Opcode.Get_font_character { index = 10 });
      Set_index_register_to_state_region { offset = 11 };
    ]
  in
  flatten_subroutines_exn ~start_memory_location:0 opcodes
  |> List.iteri ~f:(fun i opcode ->
         let memory_location = i * Constants.bytes_per_opcode in
         let opcode =
           Allowed_in_subroutine.sexp_of_t opcode |> Sexp.to_string
         in
         [%string "%{memory_location#Int}: %{opcode}"] |> print_endline);
  [%expect
    {|
    0: (Set_index_register_to_draw_region(offset 0))
    2: (Jump(relative_to_self 1))
    4: (Finalized(Subroutine_start(memory_location 18)))
    6: (Finalized(Get_key(index 8)))
    8: (Finalized(Subroutine_start(memory_location 28)))
    10: (Finalized Clear_screen)
    12: (Finalized(Get_font_character(index 10)))
    14: (Set_index_register_to_state_region(offset 11))
    16: (Finalized Halt)
    18: (Finalized(Add_to_register(index 2)(to_add(Direct 3))))
    20: (Finalized(Set_index_register(value 4)))
    22: (Finalized(Draw(x_index 5)(y_index 6)(num_bytes 7)))
    24: (Finalized Subroutine_end)
    26: (Finalized Halt)
    28: (Finalized(Get_key(index 9)))
    30: (Finalized Subroutine_end)
    32: (Finalized Halt)
    |}]

let%expect_test "[flatten_subroutines_exn] - nested subroutine" =
  let subroutine =
    Opcode.
      [
        Subroutine_start { memory_location = 0 };
        Set_index_register { value = 1 };
        Subroutine_end;
      ]
    |> List.map ~f:Allowed_in_subroutine.finalized
  in
  let input = [ Subroutine_body subroutine ] in
  let result =
    Or_error.try_with (fun () ->
        let output = flatten_subroutines_exn ~start_memory_location:0 input in
        [%message
          "BUG! Nested subroutine did not raise"
            (input : t list)
            (output : Allowed_in_subroutine.t list)])
  in
  let () =
    match result with
    | Error error -> print_s [%message (error : Error.t)]
    | Ok sexp -> print_s sexp
  in
  [%expect
    {|
    (error
     ("Nested subroutines are not supported"
      (subroutine
       (Subroutine_body
        ((Finalized (Subroutine_start (memory_location 0)))
         (Finalized (Set_index_register (value 1))) (Finalized Subroutine_end))))
      (ts
       ((Subroutine_body
         ((Finalized (Subroutine_start (memory_location 0)))
          (Finalized (Set_index_register (value 1))) (Finalized Subroutine_end)))))))
    |}]

let finalize_all_exn ~scratch_bytes_for_draw ts =
  let ts = flatten_subroutines_exn ts in
  let start_of_draw_region =
    let num_opcodes = List.length ts in
    Constants.program_start_memory_location
    + (num_opcodes * Constants.bytes_per_opcode)
  in
  let start_of_state_region = start_of_draw_region + scratch_bytes_for_draw in
  List.mapi ts ~f:(fun i opcode ->
      let program_counter = Constants.program_start_memory_location + (i * 2) in
      match opcode with
      | Finalized opcode -> opcode
      | Jump { relative_to_self } ->
          Jump
            {
              new_program_counter_base = program_counter + relative_to_self;
              with_offset = false;
            }
      | Set_index_register_to_draw_region { offset } ->
          Set_index_register { value = start_of_draw_region + offset }
      | Set_index_register_to_state_region { offset } ->
          Set_index_register { value = start_of_state_region + offset })
