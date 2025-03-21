open! Core
open! Import

module Constants = struct
  include Constants

  let bytes_per_opcode = 2
end

type t =
  | Finalized of Opcode.t
  | Jump of { relative_to_self : int }
  | Set_index_register_to_draw_region of { offset : int }
  | Set_index_register_to_state_region of { offset : int }
  | Subroutine_body of Opcode.t list
[@@deriving variants]

module Flattened = struct
  type t =
    | Finalized of Opcode.t
    | Jump of { relative_to_self : int }
    | Set_index_register_to_draw_region of { offset : int }
    | Set_index_register_to_state_region of { offset : int }
  [@@deriving sexp_of, variants]
end

module Context = struct
  type t = { main_body : Flattened.t list; subroutines : Flattened.t list }

  let empty = { main_body = []; subroutines = [] }

  (* this is horribly inefficient, but the number of opcodes in any program
     I write should be sufficiently small to make this not matter! *)
  let append { main_body; subroutines } ~flattened ~subroutine =
    {
      main_body = main_body @ [ flattened ];
      subroutines = subroutines @ subroutine;
    }

  let to_opcodes { main_body; subroutines } = main_body @ subroutines
end

(* Every [Subroutine_body opcodes] is flattened mapped into a [Subroutine_start] in the main body
   and [opcodes; Subroutine_end; Halt] in the subroutine memory region *)
let flatten_subroutines
    ?(start_memory_location = Constants.program_start_memory_location) ts =
  (* adding a [Halt] effectively spaces the main body with the subroutine region *)
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
      let (flattened : Flattened.t), subroutine =
        match t with
        | Finalized opcode -> (Finalized opcode, [])
        | Jump { relative_to_self } -> (Jump { relative_to_self }, [])
        | Set_index_register_to_draw_region { offset } ->
            (Set_index_register_to_draw_region { offset }, [])
        | Set_index_register_to_state_region { offset } ->
            (Set_index_register_to_state_region { offset }, [])
        | Subroutine_body opcodes ->
            let simplified =
              let next_subroutine_start = next_subroutine_start context in
              Opcode.Subroutine_start
                { memory_location = next_subroutine_start }
              |> Flattened.Finalized
            in
            let subroutine =
              (opcodes @ Opcode.[ Subroutine_end; Halt ])
              |> List.map ~f:Flattened.finalized
            in
            (simplified, subroutine)
      in
      Context.append context ~flattened ~subroutine)
  |> Context.to_opcodes

let%expect_test "[flatten_subroutines]" =
  let opcodes =
    [
      Set_index_register_to_draw_region { offset = 0 };
      Jump { relative_to_self = 1 };
      Subroutine_body
        Opcode.
          [
            Add_to_register { index = 2; to_add = Direct 3 };
            Set_index_register { value = 4 };
            Draw { x_index = 5; y_index = 6; num_bytes = 7 };
          ];
      Finalized (Opcode.Get_key { index = 8 });
      Subroutine_body Opcode.[ Get_key { index = 9 } ];
      Finalized Opcode.Clear_screen;
      Finalized (Opcode.Get_font_character { index = 10 });
      Set_index_register_to_state_region { offset = 11 };
    ]
  in
  flatten_subroutines ~start_memory_location:0 opcodes
  |> List.iteri ~f:(fun i opcode ->
         let memory_location = i * Constants.bytes_per_opcode in
         let opcode = Flattened.sexp_of_t opcode |> Sexp.to_string in
         [%string "%{memory_location#Int}: %{opcode}"] |> print_endline);
  [%expect {|
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

let finalize_all ~scratch_bytes_for_draw ts =
  let ts = flatten_subroutines ts in
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
