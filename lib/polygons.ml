open Conv
open OCADml

module Vert = struct
  type t =
    { pos : v2
    ; idx : int
    }
end

module Simple = struct
  let size = C.Funcs.polygons_size () |> size_to_int
  let destruct t = C.Funcs.destruct_simple_polygon t

  let alloc () =
    let finalise = Mem.finaliser C.Types.SimplePolygon.t destruct in
    let buf = Mem.allocate_buf ~finalise size in
    buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.SimplePolygon.t) buf)

  let make path =
    let buf, t = alloc ()
    and len = List.length path in
    let ps = Ctypes.CArray.make C.Types.Vec2.t len in
    List.iteri (fun i p -> Ctypes.CArray.set ps i (vec2_of_v2 p)) path;
    let _ = C.Funcs.simple_polygon buf (Ctypes.CArray.start ps) (size_of_int len) in
    t
end

type t = Manifold_c_types.Polygons.t Ctypes_static.ptr

let size = C.Funcs.polygons_size () |> size_to_int
let destruct t = C.Funcs.destruct_polygons t

let alloc () =
  let finalise = Mem.finaliser C.Types.Polygons.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.Polygons.t) buf)

(* FIXME: Winding needs to be forced to opposite of the OCADml convention (e.g.
    CW rather than CCW) for creating meshes with CW faces (there is no such
    check in Manifold at this time). *)

let make paths =
  let buf, t = alloc ()
  and len = List.length paths in
  let simps = Ctypes.CArray.make (Ctypes_static.ptr C.Types.SimplePolygon.t) len in
  List.iteri (fun i p -> Ctypes.CArray.set simps i (Simple.make p)) paths;
  let _ = C.Funcs.polygons buf (Ctypes.CArray.start simps) (size_of_int len) in
  t

let of_poly2 p = make (p.Poly2.outer :: p.holes)

let of_poly2s polys =
  let f ps poly = poly.Poly2.outer :: List.rev_append poly.holes ps in
  make @@ List.fold_left f [] polys

let of_path p = make [ p ]
let of_paths p = make p
