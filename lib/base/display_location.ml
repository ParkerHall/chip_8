open! Core

module T = struct
  type t = { x : int; y : int } [@@deriving compare, sexp]

  (* N.B. phall (2025-01-27): By convention, the chip-8 display places
    the origin at the top-left corner of the screen. The OCaml [Graphics]
    module uses the bottom-left corner as the origin, so we must flip the
    [y] value. *)
  let create ~x ~y = { x; y = Constants.display_pixel_height - y - 1 }
end

include T
include Comparable.Make (T)
