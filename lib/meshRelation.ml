open OCADml
open Conv

type ptr = C.Types.MeshRelation.t Ctypes_static.ptr

type t =
  { barycentric : v3 list
  ; tri_bary : BaryRef.t list
  }

let size = C.Funcs.mesh_relation_size () |> size_to_int
let destruct t = C.Funcs.destruct_mesh_relation t

let alloc () =
  let finalise = Mem.finaliser C.Types.MeshRelation.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.MeshRelation.t) buf)

let barycentric_length t = C.Funcs.mesh_relation_barycentric_length t
let tri_bary_length t = C.Funcs.mesh_relation_tri_bary_length t

let barycentric t =
  let len = size_to_int @@ barycentric_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof C.Types.Vec3.t)) in
  let bary = C.Funcs.mesh_relation_barycentric buf t in
  List.init len Ctypes.(fun i -> vec3_to_v3 !@(bary +@ i))

let tri_bary t =
  let len = size_to_int @@ tri_bary_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof C.Types.BaryRef.t)) in
  let tri = C.Funcs.mesh_relation_tri_bary buf t in
  List.init len Ctypes.(fun i -> BaryRef.of_struct !@(tri +@ i))

let of_ptr p = { barycentric = barycentric p; tri_bary = tri_bary p }
