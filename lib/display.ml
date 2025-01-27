open! Core

module Constants = struct
  let pixel_size = 10
  let width = 64 * pixel_size
  let height = 32 * pixel_size
end

module Location = struct
  module T = struct
    type t = { x : int; y : int } [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type t = { max_x : int; max_y : int; set : Location.Set.t }

let init () =
  Graphics.open_graph "";
  Graphics.resize_window Constants.width Constants.height;
  {
    max_x = Constants.width - 1;
    max_y = Constants.height - 1;
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

let is_set t ~x ~y = Set.mem t.set { Location.x; y }

let flip t ~x ~y =
  match x < 0 || x > t.max_x || y < 0 || y > t.max_y with
  | true -> (t, `Out_of_bounds)
  | false -> (
      match is_set t ~x ~y with
      | true -> (unset t ~x ~y, `Unset)
      | false -> (set t ~x ~y, `Set))

let clear t =
  let () = Graphics.clear_graph () in
  { t with set = Location.Set.empty }

let run_test () =
  let t = init () in
  let t =
    List.init 20 ~f:Fn.id |> List.fold ~init:t ~f:(fun t i -> set t ~x:i ~y:i)
  in
  Graphics.read_key () |> ignore;
  let (_ : t) =
    List.init 10 ~f:Fn.id |> List.fold ~init:t ~f:(fun t i -> unset t ~x:i ~y:i)
  in
  Graphics.read_key () |> ignore
