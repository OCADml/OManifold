open Conv

type t = C.Types.Mesh.t Ctypes_static.ptr

let size = C.Funcs.mesh_size () |> size_to_int
let destruct t = C.Funcs.destruct_mesh t

let alloc () =
  let finalise = Mem.finaliser C.Types.Mesh.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.Mesh.t) buf

let copy t =
  let buf, fresh = alloc () in
  let _ = C.Funcs.mesh_copy buf t in
  fresh

let vert_length t = C.Funcs.mesh_vert_length t
let tri_length t = C.Funcs.mesh_tri_length t
let normal_length t = C.Funcs.mesh_normal_length t
let tangent_length t = C.Funcs.mesh_tangent_length t

let points t =
  let len = size_to_int @@ vert_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof C.Types.Vec3.t)) in
  let verts = C.Funcs.mesh_vert_pos buf t in
  List.init len Ctypes.(fun i -> vec3_to_v3 !@(verts +@ i))

let tri_verts of_ivec3 t =
  let len = size_to_int @@ tri_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof C.Types.IVec3.t)) in
  let verts = C.Funcs.mesh_tri_verts buf t in
  List.init len Ctypes.(fun i -> of_ivec3 !@(verts +@ i))

let[@inline] triangles t = tri_verts ivec3_to_tup t
let[@inline] faces t = tri_verts ivec3_to_list t

let normals t =
  let len = size_to_int @@ normal_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof C.Types.Vec3.t)) in
  let normals = C.Funcs.mesh_vert_normal buf t in
  List.init len Ctypes.(fun i -> vec3_to_v3 !@(normals +@ i))

let halfedge_tangents t =
  let len = size_to_int @@ tangent_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof C.Types.Vec3.t)) in
  let tangents = C.Funcs.mesh_halfedge_tangent buf t in
  List.init len Ctypes.(fun i -> vec4_to_v4 !@(tangents +@ i))
