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
  let _ = C.Funcs.box buf p1.x p1.y p1.z p2.x p2.y p2.z in
  t

(* Properties *)

let min t = vec3_to_v3 @@ C.Funcs.box_min t
let max t = vec3_to_v3 @@ C.Funcs.box_max t
let center t = vec3_to_v3 @@ C.Funcs.box_center t
let dimensions t = vec3_to_v3 @@ C.Funcs.box_dimensions t
let abs_max_coord t = C.Funcs.box_scale t
let contains_pt t (p : v3) = C.Funcs.box_contains_pt t p.x p.y p.z > 0
let contains_box a b = C.Funcs.box_contains_box a b > 0
let overlaps_pt t (p : v3) = C.Funcs.box_does_overlap_pt t p.x p.y p.z > 0
let overlaps_box a b = C.Funcs.box_does_overlap_box a b > 0
let is_finite t = C.Funcs.box_is_finite t > 0

(* Transformations *)

let union a b =
  let buf, t = alloc () in
  let _ = C.Funcs.box_union buf a b in
  t

let include_pt t (p : v3) = C.Funcs.box_include_pt t p.x p.y p.z

let translate (p : v3) t =
  let buf, translated = alloc () in
  let _ = C.Funcs.box_translate buf t p.x p.y p.z in
  translated

let scale (s : v3) t =
  let buf, scaled = alloc () in
  let _ = C.Funcs.box_mul buf t s.x s.y s.z in
  scaled

let transform (a : Affine3.t) t =
  let buf, transformed = alloc () in
  let _ =
    C.Funcs.box_transform
      buf
      t
      a.r0c0
      a.r1c0
      a.r2c0
      a.r0c1
      a.r1c1
      a.r2c1
      a.r0c2
      a.r1c2
      a.r2c2
      a.r0c3
      a.r1c3
      a.r2c3
  in
  transformed

(* OCADml Conversion *)

let of_bbox bb = make bb.V3.min bb.max
let to_bbox t = V3.{ min = min t; max = max t }
