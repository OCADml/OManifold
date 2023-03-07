open OCADml
open Conv

type t = C.Types.Rect.t Ctypes_static.ptr

let size = C.Funcs.rect_size () |> size_to_int
let destruct t = C.Funcs.destruct_rect t

let alloc () =
  let finalise = Mem.finaliser C.Types.Rect.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.Rect.t) buf

let make (p1 : v2) (p2 : v2) =
  let buf, t = alloc () in
  let _ = V2.(C.Funcs.rect buf (x p1) (y p1) (x p2) (y p2)) in
  t

(* Properties *)

let min t = vec2_to_v2 @@ C.Funcs.rect_min t
let max t = vec2_to_v2 @@ C.Funcs.rect_max t
let center t = vec2_to_v2 @@ C.Funcs.rect_center t
let dimensions t = vec2_to_v2 @@ C.Funcs.rect_dimensions t
let abs_max_coord t = C.Funcs.rect_scale t
let contains_pt t p = V2.(C.Funcs.rect_contains_pt t (x p) (y p))
let contains_rect a b = C.Funcs.rect_contains_rect a b
let overlaps_rect a b = C.Funcs.rect_does_overlap_rect a b
let is_empty t = C.Funcs.rect_is_empty t
let is_finite t = C.Funcs.rect_is_finite t

(* Transformations *)

let union a b =
  let buf, t = alloc () in
  let _ = C.Funcs.rect_union buf a b in
  t

let include_pt t p = V2.(C.Funcs.rect_include_pt t (x p) (y p))

let translate p t =
  let buf, translated = alloc () in
  let _ = V2.(C.Funcs.rect_translate buf t (x p) (y p)) in
  translated

let scale s t =
  let buf, scaled = alloc () in
  let _ = V2.(C.Funcs.rect_mul buf t (x s) (y s)) in
  scaled

let transform a t =
  let buf, transformed = alloc () in
  let _ =
    let open Gg.M3 in
    C.Funcs.rect_transform buf t (e00 a) (e10 a) (e01 a) (e11 a) (e02 a) (e12 a)
  in
  transformed

(* Conversion *)

let of_box bb = make (Box2.min bb) (Box2.max bb)
let to_box t = Box2.v (min t) (max t)
