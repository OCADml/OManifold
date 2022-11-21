open Conv

type t = C.Types.Mesh.t Ctypes_static.ptr

let size = C.Funcs.mesh_size () |> size_to_int
let destruct t = C.Funcs.destruct_mesh t

let alloc () =
  let finalise = Mem.finaliser C.Types.Mesh.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.Mesh.t) buf
