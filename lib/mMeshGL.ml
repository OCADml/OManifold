open Conv

type t = C.Types.MeshGL.t Ctypes_static.ptr

let size = C.Funcs.meshgl_size () |> size_to_int
let destruct t = C.Funcs.destruct_meshgl t

let alloc () =
  let finalise = Mem.finaliser C.Types.MeshGL.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.MeshGL.t) buf

(* ManifoldMeshGL *manifold_meshgl_copy(void *mem, ManifoldMeshGL *m); *)
(* size_t manifold_meshgl_vert_length(ManifoldMeshGL *m); *)
(* size_t manifold_meshgl_tri_length(ManifoldMeshGL *m); *)
(* size_t manifold_meshgl_normal_length(ManifoldMeshGL *m); *)
(* size_t manifold_meshgl_tangent_length(ManifoldMeshGL *m); *)
(* float *manifold_meshgl_vert_pos(void *mem, ManifoldMeshGL *m); *)
(* uint32_t *manifold_meshgl_tri_verts(void *mem, ManifoldMeshGL *m); *)
(* float *manifold_meshgl_vert_normal(void *mem, ManifoldMeshGL *m); *)
(* float *manifold_meshgl_halfedge_tangent(void *mem, ManifoldMeshGL *m); *)
