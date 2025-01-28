open! Core

module Constants = struct
  let pixel_size = 10
  let pixel_width = 64
  let pixel_height = 32
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

type t = { max_x : int; max_y : int; set : Location.Set.t }

let init () =
  Graphics.open_graph "";
  Graphics.resize_window Constants.width Constants.height;
  {
    max_x = Constants.pixel_width - 1;
    max_y = Constants.pixel_height - 1;
    set = Location.Set.empty;
  }

let fill_pixel ~x ~y ~color =
  Graphics.set_color color;
  Graphics.fill_rect (x * Constants.pixel_size) (y * Constants.pixel_size)
    Constants.pixel_size Constants.pixel_size

let set t ~x ~y =
  let () = fill_pixel ~x ~y ~color:Graphics.black in
  { t with set = Set.add t.set { Location.x; y } }

let unset t ~x ~y =
  let () = fill_pixel ~x ~y ~color:Graphics.white in
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
  let () = Graphics.clear_graph () in
  { t with set = Location.Set.empty }

module Testing = struct
  let freeze (_ : t) = Graphics.read_key () |> ignore
end
