open! Core
open! Import

module Constants = struct
  let pixel_size = 10
  let pixel_width = Constants.display_pixel_width
  let pixel_height = Constants.display_pixel_height
  let width = pixel_width * pixel_size
  let height = pixel_height * pixel_size
end

module Location = struct
  module T = struct
    type t = { x : int; y : int } [@@deriving compare, sexp]

    (* N.B. phall (2025-01-27): By convention, the chip-8 display places
    the origin at the top-left corner of the screen. The OCaml [Graphics]
    module uses the bottom-left corner as the origin, so we must flip the
    [y] value. *)
    let create ~x ~y = { x; y = Constants.pixel_height - y - 1 }
  end

  include T
  include Comparable.Make (T)
end

type t = {
  max_x : int;
  max_y : int;
  set : Location.Set.t;
  disable_graphics : bool;
}

let maybe_do_graphics t ~f = if not t.disable_graphics then f ()

let init' ~disable_graphics =
  {
    max_x = Constants.pixel_width - 1;
    max_y = Constants.pixel_height - 1;
    set = Location.Set.empty;
    disable_graphics;
  }

let init () =
  let t = init' ~disable_graphics:false in
  maybe_do_graphics t ~f:(fun () ->
      Graphics.open_graph "";
      Graphics.resize_window Constants.width Constants.height);
  t

let fill_pixel ~x ~y ~color () =
  Graphics.set_color color;
  Graphics.fill_rect (x * Constants.pixel_size) (y * Constants.pixel_size)
    Constants.pixel_size Constants.pixel_size

let set t ~x ~y =
  maybe_do_graphics t ~f:(fill_pixel ~x ~y ~color:Graphics.black);
  { t with set = Set.add t.set { Location.x; y } }

let unset t ~x ~y =
  maybe_do_graphics t ~f:(fill_pixel ~x ~y ~color:Graphics.white);
  { t with set = Set.remove t.set { Location.x; y } }

let is_set t location = Set.mem t.set location

let flip t ({ Location.x; y } as loc) =
  match x < 0 || x > t.max_x || y < 0 || y > t.max_y with
  | true -> (t, `Out_of_bounds)
  | false -> (
      match is_set t loc with
      | true -> (unset t ~x ~y, `Unset)
      | false -> (set t ~x ~y, `Set))

let clear t =
  maybe_do_graphics t ~f:Graphics.clear_graph;
  { t with set = Location.Set.empty }

let freeze t = maybe_do_graphics t ~f:(fun () -> Graphics.read_key () |> ignore)

module Testing = struct
  let init_no_graphics () = init' ~disable_graphics:true

  let dump_to_stdout t =
    let ys = List.init Constants.pixel_height ~f:Fn.id in
    let xs = List.init Constants.pixel_width ~f:Fn.id in
    List.cartesian_product ys xs
    |> List.iter ~f:(fun (y, x) ->
           let location = Location.create ~x ~y in
           let char = if is_set t location then "#" else " " in
           match x = Constants.pixel_width - 1 with
           | true -> print_endline char
           | false -> print_string char)
end
