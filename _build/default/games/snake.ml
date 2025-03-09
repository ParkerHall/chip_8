open! Core
open! Import

module Display_title = struct
  let pad bytes = List.map bytes ~f:(fun byte -> byte lsl 4)
  let s = [ 0b0110; 0b1001; 0b0100; 0b0010; 0b1001; 0b0110 ] |> pad
  let n = [ 0b1001; 0b1001; 0b1101; 0b1011; 0b1001; 0b1001 ] |> pad
  let a = [ 0b0110; 0b1001; 0b1111; 0b1001; 0b1001; 0b1001 ] |> pad
  let k = [ 0b1001; 0b1010; 0b1100; 0b1100; 0b1010; 0b1001 ] |> pad
  let e = [ 0b1111; 0b1000; 0b1111; 0b1000; 0b1000; 0b1111 ] |> pad
  let title = [ s; n; a; k; e ]

  let opcodes ~memory_start_location =
    let start_x =
      let total_width =
        (List.length title * Constants.bits_in_byte)
        (* ignores the second half of E, which is whitespace *)
        - (Constants.bits_in_byte / 2)
      in
      (Constants.display_pixel_width - total_width) / 2
    in
    (* [s] is arbitrary; since all characters are the same height, any would do *)
    let y = (Constants.display_pixel_height - List.length s) / 2 in
    List.concat_mapi title ~f:(fun i character ->
        let set_index_register =
          Opcode.Set_index_register { value = memory_start_location }
        in
        let set_registers =
          List.mapi character ~f:(fun index byte ->
              Opcode.Set_register { index; to_ = Non_timer (Direct byte) })
        in
        let store_into_memory =
          Opcode.Store { up_to_index = List.length character - 1 }
        in
        let draw =
          let x = start_x + (i * Constants.bits_in_byte) + 1 in
          [
            Opcode.Set_register { index = 0; to_ = Non_timer (Direct x) };
            Opcode.Set_register { index = 1; to_ = Non_timer (Direct y) };
            Opcode.Draw
              { x_index = 0; y_index = 1; num_bytes = List.length character };
          ]
        in
        [ [ set_index_register ]; set_registers; [ store_into_memory ]; draw ]
        |> List.concat)
end

let opcodes = Display_title.opcodes ~memory_start_location:0x200
