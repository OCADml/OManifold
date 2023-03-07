open! OCADml
open Conv

type t = C.Types.CrossSection.t Ctypes_static.ptr

let size = C.Funcs.cross_section_size () |> size_to_int
let destruct t = C.Funcs.destruct_cross_section t

let alloc () =
  let finalise = Mem.finaliser C.Types.CrossSection.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  ( buf
  , Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.CrossSection.t) buf
  )

(* TODO: add empty and copy constructors *)
(* let empty () = *)
(*   let buf, t = alloc () in *)
(*   let _ = C.Funcs.cross_section_empty buf in *)
(*   t *)
(* let copy t = *)
(*   let buf, cs = alloc () in *)
(*   let _ = C.Funcs.cross_section_copy buf t in *)
(*   cs *)

let bounds t =
  let buf, rect = MRect.alloc () in
  let _ = C.Funcs.cross_section_bounds buf t in
  rect

let of_simple_polygon ?(fill_rule = `Positive) sp =
  let buf, t = alloc ()
  and fill_rule = FillRule.make fill_rule in
  let _ = C.Funcs.cross_section_of_simple_polygon buf sp fill_rule in
  t

let of_polygons ?(fill_rule = `Positive) ps =
  let buf, t = alloc ()
  and fill_rule = FillRule.make fill_rule in
  let _ = C.Funcs.cross_section_of_polygons buf ps fill_rule in
  t

let circle ?(fn = 0) rad =
  let buf, t = alloc () in
  let _ = C.Funcs.cross_section_circle buf rad fn in
  t

let square ?(center = false) d =
  let buf, t = alloc () in
  let _ = V3.(C.Funcs.cross_section_square buf (x d) (y d) center) in
  t

let add a b =
  let buf, t = alloc () in
  let _ = C.Funcs.cross_section_union buf a b in
  t

let sub a b =
  let buf, t = alloc () in
  let _ = C.Funcs.cross_section_difference buf a b in
  t

let intersect a b =
  let buf, t = alloc () in
  let _ = C.Funcs.cross_section_intersection buf a b in
  t

let xor a b =
  let buf, t = alloc () in
  let _ = C.Funcs.cross_section_xor buf a b in
  t

let union = function
  | [] -> empty ()
  | [ a ] -> copy a
  | [ a; b ] -> add a b
  | a :: ts -> List.fold_left (fun t e -> add t e) a ts

let difference t = function
  | [] -> copy t
  | ts -> List.fold_left (fun t e -> sub t e) t ts

let intersection = function
  | [] -> empty ()
  | [ a ] -> copy a
  | a :: ts -> List.fold_left (fun t e -> intersect t e) a ts

let xor = function
  | [] -> empty ()
  | [ a ] -> copy a
  | a :: ts -> List.fold_left (fun t e -> xor t e) a ts

let rect_clip t rect =
  let buf, clipped = alloc () in
  let _ = V2.(C.Funcs.cross_section_rect_clip buf t rect) in
  clipped

let translate p t =
  let buf, translated = alloc () in
  let _ = V2.(C.Funcs.cross_section_translate buf t (x p) (y p)) in
  translated

let[@inline] xtrans x t = translate (v2 x 0.) t
let[@inline] ytrans y t = translate (v2 0. y) t

let rotate ?about r t =
  let r = Math.deg_of_rad r in
  let rot t =
    let buf, rotated = alloc () in
    let _ = C.Funcs.cross_section_rotate buf t r in
    rotated
  in
  match about with
  | None -> rot t
  | Some p -> translate (V2.neg p) t |> rot |> translate p

let[@inline] zrot z t = rotate z t

let mirror ax t =
  let buf, mirrored = alloc () in
  let _ = V2.(C.Funcs.cross_section_mirror buf t (x ax) (y ax)) in
  mirrored

let affine (a : Affine2.t) t =
  let buf, transformed = alloc () in
  let _ =
    let open Gg.M3 in
    C.Funcs.cross_section_transform buf t (e00 a) (e10 a) (e01 a) (e11 a) (e02 a) (e12 a)
  in
  transformed

let scale s t =
  let buf, scaled = alloc () in
  let _ = V2.(C.Funcs.cross_section_scale buf t (x s) (y s)) in
  scaled

let[@inline] xscale x t = scale (v2 x 1.) t
let[@inline] yscale y t = scale (v2 1. y) t

let simplify ?(eps = 1e-6) t =
  let buf, simplified = alloc () in
  let _ = V2.(C.Funcs.cross_section_simplify buf t eps) in
  simplified

let offset ?(join_type = `Square) ?(miter_limit = 2.0) ?(arc_tolerance = 0.) ~delta t =
  let buf, off = alloc ()
  and join_type = JoinType.make join_type in
  let _ = C.Funcs.cross_section_offset buf t delta join_type miter_limit arc_tolerance in
  off

let area t = C.Funcs.cross_section_area t
let is_empty t = C.Funcs.cross_section_is_empty t
