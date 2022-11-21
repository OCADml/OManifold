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
  let _ = C.Funcs.manifold_box buf p1.x p1.y p1.z p2.x p2.y p2.z in
  t

let of_bbox bb = make bb.V3.min bb.max
let min t = vec3_to_v3 @@ C.Funcs.manifold_box_min t
let max t = vec3_to_v3 @@ C.Funcs.manifold_box_max t
let center t = vec3_to_v3 @@ C.Funcs.manifold_box_center t
