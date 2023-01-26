open OCADml
open Conv

type t = C.Types.Box.t Ctypes_static.ptr

let size = C.Funcs.box_size () |> size_to_int
let destruct t = C.Funcs.destruct_box t

let alloc () =
  let finalise = Mem.finaliser C.Types.Box.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.Box.t) buf

let make (p1 : v3) (p2 : v3) =
  let buf, t = alloc () in
  let _ = V3.(C.Funcs.box buf (x p1) (y p1) (z p1) (x p2) (y p2) (z p2)) in
  t

(* Properties *)

let min t = vec3_to_v3 @@ C.Funcs.box_min t
let max t = vec3_to_v3 @@ C.Funcs.box_max t
let center t = vec3_to_v3 @@ C.Funcs.box_center t
let dimensions t = vec3_to_v3 @@ C.Funcs.box_dimensions t
let abs_max_coord t = C.Funcs.box_scale t
let contains_pt t p = V3.(C.Funcs.box_contains_pt t (x p) (y p) (z p) > 0)
let contains_box a b = C.Funcs.box_contains_box a b > 0
let overlaps_pt t p = V3.(C.Funcs.box_does_overlap_pt t (x p) (y p) (z p) > 0)
let overlaps_box a b = C.Funcs.box_does_overlap_box a b > 0
let is_finite t = C.Funcs.box_is_finite t > 0

(* Transformations *)

let union a b =
  let buf, t = alloc () in
  let _ = C.Funcs.box_union buf a b in
  t

let include_pt t p = V3.(C.Funcs.box_include_pt t (x p) (y p) (z p))

let translate p t =
  let buf, translated = alloc () in
  let _ = V3.(C.Funcs.box_translate buf t (x p) (y p) (z p)) in
  translated

let scale s t =
  let buf, scaled = alloc () in
  let _ = V3.(C.Funcs.box_mul buf t (x s) (y s) (z s)) in
  scaled

let transform a t =
  let buf, transformed = alloc () in
  let _ =
    let open Gg.M4 in
    C.Funcs.box_transform
      buf
      t
      (e00 a)
      (e10 a)
      (e20 a)
      (e01 a)
      (e11 a)
      (e21 a)
      (e02 a)
      (e12 a)
      (e22 a)
      (e03 a)
      (e13 a)
      (e23 a)
  in
  transformed

(* OCADml Conversion *)

let of_box bb = make (Box3.min bb) (Box3.max bb)
let to_box t = Box3.v (min t) (max t)
