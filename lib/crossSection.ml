open! OCADml
open Conv

type t = C.Types.CrossSection.t Ctypes_static.ptr

type fill_rule =
  [ `EvenOdd
  | `Negative
  | `NonZero
  | `Positive
  ]

type join_type =
  [ `Square
  | `Round
  | `Miter
  ]

let size = C.Funcs.cross_section_size () |> size_to_int
let destruct t = C.Funcs.destruct_cross_section t

let alloc () =
  let finalise = Mem.finaliser C.Types.CrossSection.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  ( buf
  , Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.CrossSection.t) buf
  )

module Vec = struct
  let size = C.Funcs.cross_section_size () |> size_to_int
  let destruct t = C.Funcs.destruct_cross_section_vec t

  let alloc' () =
    let finalise = Mem.finaliser C.Types.CrossSectionVec.t destruct in
    let buf = Mem.allocate_buf ~finalise size in
    buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.CrossSectionVec.t) buf)

  let empty () =
    let buf, t = alloc' () in
    let _ = C.Funcs.cross_section_empty_vec buf in
    t

  let reserve t n = C.Funcs.cross_section_vec_reserve t n

  let make n =
    let buf, t = alloc' () in
    let _ = C.Funcs.cross_section_vec buf (size_of_int n) in
    t

  let length t = size_to_int @@ C.Funcs.cross_section_vec_length t

  let get t i =
    let buf, m = alloc () in
    let _ = C.Funcs.cross_section_vec_get buf t i in
    m

  let set t i m = C.Funcs.cross_section_vec_set t i m
  let push_back t m = C.Funcs.cross_section_vec_push_back t m
end

let empty () =
  let buf, t = alloc () in
  let _ = C.Funcs.cross_section_empty buf in
  t

let copy t =
  let buf, cs = alloc () in
  let _ = C.Funcs.cross_section_copy buf t in
  cs

let bounds t =
  let buf, rect = MRect.alloc () in
  let _ = C.Funcs.cross_section_bounds buf t in
  MRect.to_box rect

let of_simple_polygon ?(fill_rule = `Positive) sp =
  let buf, t = alloc ()
  and fill_rule = FillRule.make fill_rule in
  let _ = C.Funcs.cross_section_of_simple_polygon buf sp fill_rule in
  t

let of_path ?fill_rule p = of_simple_polygon ?fill_rule (Polygons.Simple.make p)

let of_polygons ?(fill_rule = `Positive) ps =
  let buf, t = alloc ()
  and fill_rule = FillRule.make fill_rule in
  let _ = C.Funcs.cross_section_of_polygons buf ps fill_rule in
  t

let of_paths ?fill_rule ps = of_polygons ?fill_rule (Polygons.of_paths ps)
let of_poly2 ?fill_rule p = of_polygons ?fill_rule (Polygons.of_poly2 p)
let of_poly2s ?fill_rule ps = of_polygons ?fill_rule (Polygons.of_poly2s ps)

let circle ?(fn = 0) rad =
  let buf, t = alloc () in
  let _ = C.Funcs.cross_section_circle buf rad fn in
  t

let square ?(center = false) d =
  let buf, t = alloc () in
  let _ = V2.(C.Funcs.cross_section_square buf (x d) (y d) center) in
  t

let compose ts =
  let buf, t = alloc () in
  let csv = Vec.empty () in
  List.iter (fun t -> Vec.push_back csv t) ts;
  let _ = C.Funcs.cross_section_compose buf csv in
  t

let decompose t =
  let buf, csv = Vec.alloc' () in
  let _ = C.Funcs.cross_section_decompose buf t
  and ts = ref [] in
  for i = Vec.length csv - 1 downto 0 do
    let buf, cs = alloc () in
    let _ = C.Funcs.cross_section_vec_get buf csv i in
    ts := cs :: !ts
  done;
  !ts

(* Booleans *)

let boolean ~op a b =
  let buf, t = alloc ()
  and op = OpType.make op in
  let _ = C.Funcs.cross_section_boolean buf a b op in
  t

let batch_boolean ~op ts =
  let buf, t = alloc ()
  and op = OpType.make op
  and csv = Vec.empty () in
  List.iter (fun m -> Vec.push_back csv m) ts;
  let _ = C.Funcs.cross_section_batch_boolean buf csv op in
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

let union = function
  | [] -> empty ()
  | [ a ] -> copy a
  | [ a; b ] -> add a b
  | ts -> batch_boolean ~op:`Add ts

let difference t = function
  | [] -> copy t
  | ts -> batch_boolean ~op:`Subtract ts

let intersection = function
  | [] -> empty ()
  | [ a ] -> copy a
  | ts -> batch_boolean ~op:`Intersect ts

let rect_clip t rect =
  let buf, clipped = alloc ()
  and rect = MRect.of_box rect in
  let _ = C.Funcs.cross_section_rect_clip buf t rect in
  clipped

(* Transforms *)
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

let[@inline] zrot ?about z t = rotate ?about z t

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

let warp f t =
  let buf, warped = alloc () in
  let f x y = Conv.vec2_of_v2 (f (v2 x y)) in
  let f =
    Ctypes.(coerce (Foreign.funptr C.Funcs.warp2_t) (static_funptr C.Funcs.warp2_t) f)
  in
  let _ = C.Funcs.cross_section_warp buf t f in
  warped

(* Simplification and Offsetting *)

let simplify ?(eps = 1e-6) t =
  let buf, simplified = alloc () in
  let _ = C.Funcs.cross_section_simplify buf t eps in
  simplified

let offset ?(join_type = `Square) ?(miter_limit = 2.0) ?(arc_tolerance = 0.) ~delta t =
  let buf, off = alloc ()
  and join_type = JoinType.make join_type in
  let _ = C.Funcs.cross_section_offset buf t delta join_type miter_limit arc_tolerance in
  off

(* Information *)

let area t = C.Funcs.cross_section_area t
let num_vert t = C.Funcs.cross_section_num_vert t
let num_contour t = C.Funcs.cross_section_num_contour t
let is_empty t = C.Funcs.cross_section_is_empty t

let to_polygons t =
  let buf, ps = Polygons.alloc () in
  let _ = C.Funcs.cross_section_to_polygons buf t in
  ps

let to_paths t = Polygons.to_paths @@ to_polygons t

(* TODO: to_polys (using decompose) *)
