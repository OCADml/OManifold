open OCADml
open Conv

let size = C.Funcs.manifold_size () |> size_to_int
let destruct m = C.Funcs.destruct_manifold m

type t = C.Types.Manifold.t Ctypes_static.ptr

let pair_to_tup p = C.Types.ManifoldPair.(Ctypes.getf p first, Ctypes.getf p second)

let alloc () =
  let finalise = Mem.finaliser C.Types.Manifold.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.Manifold.t) buf

let empty () =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_empty buf in
  t

let copy t =
  let buf, man = alloc () in
  let _ = C.Funcs.manifold_copy buf t in
  man

let as_original t =
  let buf, man = alloc () in
  let _ = C.Funcs.manifold_as_original buf t in
  man

let status t = C.Funcs.manifold_status t
let original_id t = C.Funcs.manifold_original_id t
let num_vert t = C.Funcs.manifold_num_vert t
let num_edge t = C.Funcs.manifold_num_edge t
let num_tri t = C.Funcs.manifold_num_tri t

let bounding_box t =
  let buf, box = Box.alloc () in
  let _ = C.Funcs.manifold_bounding_box buf t in
  box

let precision t = C.Funcs.manifold_precision t
let genus t = C.Funcs.manifold_genus t
let get_circular_segments t r = C.Funcs.manifold_get_circular_segments t r

(* ManifoldCurvature *manifold_get_curvature(void *mem, ManifoldManifold *m); *)

let tetrahedron () =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_tetrahedron buf in
  t

let sphere ?(fn = 32) rad =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_sphere buf rad fn in
  t

let cube ?(center = false) { x; y; z } =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_cube buf x y z (Bool.to_int center) in
  t

let cylinder ?(center = false) ?(fn = 32) ~height r =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_cylinder buf height r r fn (Bool.to_int center) in
  t

let cone ?(center = false) ?(fn = 32) ~height r1 r2 =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_cylinder buf height r1 r2 fn (Bool.to_int center) in
  t

let of_mmesh m =
  (* TODO: mesh properties type with optional provision *)
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_of_mesh buf m in
  t

let extrude ?(slices = 16) ?(twist = 0.) ?(scale = v2 1. 1.) ~height:h polys =
  (* TODO: take list of paths and build the polygons type within? Or just
    preemptively have a Polygons module in anticipation of more going in there
    other than of_paths and of_poly2? *)
  let buf, t = alloc ()
  and tw = Math.deg_of_rad twist in
  (* TODO: no default for slices and calculate based on twist *)
  let _ = C.Funcs.manifold_extrude buf polys h slices tw scale.x scale.y in
  t

let revolve ?(fn = 32) polys =
  (* TODO: see extrude *)
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_revolve buf polys fn in
  t

let add a b =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_union buf a b in
  t

let sub a b =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_difference buf a b in
  t

let intersect a b =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_intersection buf a b in
  t

let union = function
  | [] -> failwith "replace with manifold_empty"
  | [ _a ] -> failwith "replace with manifold_copy"
  | [ a; b ] -> add a b
  | a :: ts -> List.fold_left (fun t e -> add t e) a ts

let difference = function
  | [] -> empty ()
  | [ a ] -> copy a
  | [ a; b ] -> sub a b
  | a :: ts -> List.fold_left (fun t e -> sub t e) a ts

let intersection = function
  | [] -> empty ()
  | [ a ] -> copy a
  | [ a; b ] -> intersect a b
  | a :: ts -> List.fold_left (fun t e -> intersect t e) a ts

let split a b =
  let buf1, first = alloc ()
  and buf2, second = alloc () in
  let _ = C.Funcs.manifold_split buf1 buf2 a b in
  first, second

let split_by_plane ({ a; b; c; d } : Plane.t) t =
  let buf1, first = alloc ()
  and buf2, second = alloc () in
  let _ = C.Funcs.manifold_split_by_plane buf1 buf2 t a b c d in
  first, second

let trim_by_plane ({ a; b; c; d } : Plane.t) t =
  let buf, trimmed = alloc () in
  let _ = C.Funcs.manifold_trim_by_plane buf t a b c d in
  trimmed

let warp f t =
  let buf, warped = alloc () in
  let f x y z = Conv.vec3_of_v3 (f (v3 x y z)) in
  let f =
    Ctypes.(coerce (Foreign.funptr C.Funcs.warp_t) (static_funptr C.Funcs.warp_t) f)
  in
  let _ = C.Funcs.manifold_warp buf t f in
  warped

let translate { x; y; z } t =
  let buf, translated = alloc () in
  let _ = C.Funcs.manifold_translate buf t x y z in
  translated

let rotate { x; y; z } t =
  let buf, rotated = alloc () in
  let _ = C.Funcs.manifold_rotate buf t x y z in
  rotated

let scale { x; y; z } t =
  let buf, scaled = alloc () in
  let _ = C.Funcs.manifold_scale buf t x y z in
  scaled

let refine n t =
  let buf, refined = alloc () in
  let _ = C.Funcs.manifold_refine buf t n in
  refined
