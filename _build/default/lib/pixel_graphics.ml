open! Core

module Constants = struct
  let pixel_size = 10
  let width = 64 * pixel_size
  let height = 32 * pixel_size
end

let init () =
  Graphics.open_graph "";
  Graphics.resize_window Constants.width Constants.height

let fill_pixel ~x ~y ~color =
  Graphics.set_color color;
  Graphics.fill_rect (x * Constants.pixel_size) (y * Constants.pixel_size)
    Constants.pixel_size Constants.pixel_size

let set ~x ~y = fill_pixel ~x ~y ~color:Graphics.black
let unset ~x ~y = fill_pixel ~x ~y ~color:Graphics.white

let run_test () =
  init ();
  List.init 20 ~f:Fn.id |> List.iter ~f:(fun i -> set ~x:i ~y:i);
  Graphics.read_key () |> ignore;
  List.init 10 ~f:Fn.id |> List.iter ~f:(fun i -> unset ~x:i ~y:i);
  Graphics.read_key () |> ignore
