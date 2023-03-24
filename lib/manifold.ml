open OCADml
open Conv

module Status = struct
  type t = C.Types.Status.t =
    | NoError
    | NonFiniteVertex
    | NotManifold
    | VertexIndexOutOfBounds
    | PropertiesWrongLength
    | MissingPositionProperties
    | MergeVectorsDifferentLengths
    | MergeIndexOutOfBounds
    | TransformWrongLength
    | RunIndexWrongLength
    | FaceIDWrongLength

  let to_string = function
    | NoError -> "NO_ERROR"
    | NonFiniteVertex -> "NON_FINITE_VERTEX"
    | NotManifold -> "NOT_MANIFOLD"
    | VertexIndexOutOfBounds -> "VERTEX_INDEX_OUT_OF_BOUNDS"
    | PropertiesWrongLength -> "PROPERTIES_WRONG_LENGTH"
    | MissingPositionProperties -> "MISSING_POSITION_PROPERTIES"
    | MergeVectorsDifferentLengths -> "MERGE_VECTORS_DIFFERENT_LENGTHS"
    | MergeIndexOutOfBounds -> "MERGE_INDEX_OUT_OF_BOUNDS"
    | TransformWrongLength -> "TRANSFORM_WRONG_LENGTH"
    | RunIndexWrongLength -> "RUN_INDEX_WRONG_LENGTH"
    | FaceIDWrongLength -> "FACE_ID_WRONG_LENGTH"
end

module Id = struct
  type t =
    | Original of int
    | Product

  let to_int = function
    | Original id -> id
    | Product -> -1
end

type size =
  { surface_area : float
  ; volume : float
  }

type t = C.Types.Manifold.t Ctypes_static.ptr

let size = C.Funcs.manifold_size () |> size_to_int
let destruct t = C.Funcs.destruct_manifold t

let alloc () =
  let finalise = Mem.finaliser C.Types.Manifold.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.Manifold.t) buf)

module Vec = struct
  let size = C.Funcs.manifold_vec_size () |> size_to_int
  let destruct t = C.Funcs.destruct_manifold_vec t

  let alloc' () =
    let finalise = Mem.finaliser C.Types.ManifoldVec.t destruct in
    let buf = Mem.allocate_buf ~finalise size in
    buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.ManifoldVec.t) buf)

  let empty () =
    let buf, t = alloc' () in
    let _ = C.Funcs.manifold_empty_vec buf in
    t

  let reserve t n = C.Funcs.manifold_vec_reserve t n

  let make n =
    let buf, t = alloc' () in
    let _ = C.Funcs.manifold_vec buf (size_of_int n) in
    t

  let length t = size_to_int @@ C.Funcs.manifold_vec_length t

  let get t i =
    let buf, m = alloc () in
    let _ = C.Funcs.manifold_vec_get buf t i in
    m

  let set t i m = C.Funcs.manifold_vec_set t i m
  let push_back t m = C.Funcs.manifold_vec_push_back t m
end

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

let original_id t =
  match C.Funcs.manifold_original_id t with
  | -1 -> Id.Product
  | id -> Original id

let num_vert t = C.Funcs.manifold_num_vert t
let num_edge t = C.Funcs.manifold_num_edge t
let num_tri t = C.Funcs.manifold_num_tri t

let bounding_box t =
  let buf, box = MBox.alloc () in
  let _ = C.Funcs.manifold_bounding_box buf t in
  box

let precision t = C.Funcs.manifold_precision t
let genus t = C.Funcs.manifold_genus t

let size t =
  let s = C.Funcs.manifold_get_properties t in
  let surface_area = Ctypes.getf s C.Types.Properties.surface_area
  and volume = Ctypes.getf s C.Types.Properties.volume in
  { surface_area; volume }

let curvature t =
  let buf, curv = Curvature.alloc () in
  let _ = C.Funcs.manifold_get_curvature buf t in
  Curvature.of_ptr curv

(* Shapes *)

let tetrahedron () =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_tetrahedron buf in
  t

let sphere ?(fn = 0) rad =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_sphere buf rad fn in
  t

let cube ?(center = false) d =
  let buf, t = alloc () in
  let _ = V3.(C.Funcs.manifold_cube buf (x d) (y d) (z d) (Bool.to_int center)) in
  t

let cylinder ?(center = false) ?(fn = 0) ~height r =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_cylinder buf height r r fn (Bool.to_int center) in
  t

let cone ?(center = false) ?(fn = 0) ~height r1 r2 =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_cylinder buf height r1 r2 fn (Bool.to_int center) in
  t

let of_mmeshgl m =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_of_meshgl buf m in
  match status t with
  | NoError -> Ok t
  | e -> Error (Status.to_string e)

let of_mmeshgl_exn m =
  match of_mmeshgl m with
  | Ok t -> t
  | Error e -> failwith (Printf.sprintf "Faiure to build Manifold from mesh: %s" e)

let smooth ?(smoothness = []) m =
  let open Ctypes in
  let buf, t = alloc () in
  let len = List.length smoothness in
  let edge_idxs = CArray.make int len
  and edge_ks = CArray.make float len in
  List.iteri
    (fun i (idx, s) ->
      CArray.set edge_idxs i idx;
      CArray.set edge_ks i s )
    smoothness;
  let idx_ptr = CArray.start edge_idxs
  and k_ptr = CArray.start edge_ks in
  let _ = C.Funcs.manifold_smooth buf m idx_ptr k_ptr (size_of_int len) in
  match status t with
  | NoError -> Ok t
  | e -> Error (Status.to_string e)

let smooth_exn ?smoothness m =
  match smooth ?smoothness m with
  | Ok t -> t
  | Error e -> failwith (Printf.sprintf "Faiure to build smooth Manifold from mesh: %s" e)

let compose ts =
  let buf, t = alloc () in
  let ms = Vec.empty () in
  List.iter (fun t -> Vec.push_back ms t) ts;
  let _ = C.Funcs.manifold_compose buf ms in
  t

let decompose t =
  let buf, mv = Vec.alloc' () in
  let _ = C.Funcs.manifold_decompose buf t
  and ts = ref [] in
  for i = Vec.length mv - 1 downto 0 do
    let buf, man = alloc () in
    let _ = C.Funcs.manifold_vec_get buf mv i in
    ts := man :: !ts
  done;
  !ts

(* Booleans *)

let boolean ~op a b =
  let buf, t = alloc ()
  and op = OpType.make op in
  let _ = C.Funcs.manifold_boolean buf a b op in
  t

let batch_boolean ~op ts =
  let buf, t = alloc ()
  and op = OpType.make op
  and mv = Vec.empty () in
  List.iter (fun m -> Vec.push_back mv m) ts;
  let _ = C.Funcs.manifold_batch_boolean buf mv op in
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

let split a b =
  let buf1, first = alloc ()
  and buf2, second = alloc () in
  let _ = C.Funcs.manifold_split buf1 buf2 a b in
  first, second

let split_by_plane plane t =
  let buf1, first = alloc ()
  and buf2, second = alloc ()
  and x, y, z, w = Plane.to_tup plane in
  let _ = C.Funcs.manifold_split_by_plane buf1 buf2 t x y z w in
  first, second

let trim_by_plane plane t =
  let buf, trimmed = alloc ()
  and x, y, z, w = Plane.to_tup plane in
  let _ = C.Funcs.manifold_trim_by_plane buf t x y z w in
  trimmed

(* Transformations *)

let warp f t =
  let buf, warped = alloc () in
  let f x y z = Conv.vec3_of_v3 (f (v3 x y z)) in
  let f =
    Ctypes.(coerce (Foreign.funptr C.Funcs.warp3_t) (static_funptr C.Funcs.warp3_t) f)
  in
  let _ = C.Funcs.manifold_warp buf t f in
  warped

let translate p t =
  let buf, translated = alloc () in
  let _ = V3.(C.Funcs.manifold_translate buf t (x p) (y p) (z p)) in
  translated

let[@inline] xtrans x t = translate (v3 x 0. 0.) t
let[@inline] ytrans y t = translate (v3 0. y 0.) t
let[@inline] ztrans z t = translate (v3 0. 0. z) t

let rotate ?about r t =
  let r = V3.deg_of_rad r in
  let rot t =
    let buf, rotated = alloc () in
    let _ = V3.(C.Funcs.manifold_rotate buf t (x r) (y r) (z r)) in
    rotated
  in
  match about with
  | None -> rot t
  | Some p -> translate (V3.neg p) t |> rot |> translate p

let[@inline] xrot ?about x t = rotate ?about (v3 x 0. 0.) t
let[@inline] yrot ?about y t = rotate ?about (v3 0. y 0.) t
let[@inline] zrot ?about z t = rotate ?about (v3 0. 0. z) t

let affine (a : Affine3.t) t =
  let buf, transformed = alloc () in
  let _ =
    let open Gg.M4 in
    C.Funcs.manifold_transform
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

let quaternion ?about q t =
  let a = Quaternion.to_affine q in
  match about with
  | None -> affine a t
  | Some p -> translate (V3.neg p) t |> affine a |> translate p

let axis_rotate ?about ax angle t = quaternion ?about (Quaternion.make ax angle) t

let scale s t =
  let buf, scaled = alloc () in
  let _ = V3.(C.Funcs.manifold_scale buf t (x s) (y s) (z s)) in
  scaled

let[@inline] xscale x t = scale (v3 x 1. 1.) t
let[@inline] yscale y t = scale (v3 1. y 1.) t
let[@inline] zscale z t = scale (v3 1. 1. z) t

let refine n t =
  let buf, refined = alloc () in
  let _ = C.Funcs.manifold_refine buf t n in
  refined

(* 2D to 3D *)

let extrude
  ?slices
  ?fa
  ?(twist = 0.)
  ?(scale = v2 1. 1.)
  ?(center = false)
  ~height:h
  cross
  =
  let buf, t = alloc ()
  and tw = Math.deg_of_rad twist in
  let slices =
    match slices, twist with
    | Some s, tw when Float.(abs tw /. (2. *. pi) < 1.) -> s
    | fn, tw -> Util.helical_slices ?fa ?fn tw
  in
  let _ = C.Funcs.manifold_extrude buf cross h slices tw (V2.x scale) (V2.y scale) in
  if center then ztrans (h /. -2.) t else t

let revolve ?(fn = 0) polys =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_revolve buf polys fn in
  t

(* Mesh Conversion *)

let to_mmeshgl t =
  let buf, mesh = MMeshGL.alloc () in
  let _ = C.Funcs.manifold_get_meshgl buf t in
  mesh

let points t = MMeshGL.points (to_mmeshgl t)
let of_mesh ?rev m = of_mmeshgl @@ MMeshGL.of_mesh ?rev m
let of_mesh_exn ?rev m = of_mmeshgl_exn @@ MMeshGL.of_mesh ?rev m
let to_mesh t = MMeshGL.to_mesh @@ to_mmeshgl t

(* Extras *)

let hull ts =
  let ps = List.fold_left (fun ps t -> List.rev_append (points t) ps) [] ts in
  of_mesh (Mesh.hull ps)

let hull_exn ts =
  match hull ts with
  | Ok t -> t
  | Error e -> failwith (Printf.sprintf "Faiure to build Manifold from hulled mesh: %s" e)
