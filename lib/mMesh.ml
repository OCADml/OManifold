open Conv

type t = C.Types.Mesh.t Ctypes_static.ptr

let size = C.Funcs.mesh_size () |> size_to_int
let destruct t = C.Funcs.destruct_mesh t

let alloc () =
  let finalise = Mem.finaliser C.Types.Mesh.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.Mesh.t) buf

(* ManifoldMesh *manifold_mesh_copy(void *mem, ManifoldMesh *m); *)
(* size_t manifold_mesh_vert_length(ManifoldMesh *m); *)
(* size_t manifold_mesh_tri_length(ManifoldMesh *m); *)
(* size_t manifold_mesh_normal_length(ManifoldMesh *m); *)
(* size_t manifold_mesh_tangent_length(ManifoldMesh *m); *)
(* ManifoldVec3 *manifold_mesh_vert_pos(void *mem, ManifoldMesh *m); *)
(* ManifoldIVec3 *manifold_mesh_tri_verts(void *mem, ManifoldMesh *m); *)
(* ManifoldVec3 *manifold_mesh_vert_normal(void *mem, ManifoldMesh *m); *)
(* ManifoldVec4 *manifold_mesh_halfedge_tangent(void *mem, ManifoldMesh *m); *)
