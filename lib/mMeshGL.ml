open Conv

type t = C.Types.MeshGL.t Ctypes_static.ptr

let size = C.Funcs.meshgl_size () |> size_to_int
let destruct t = C.Funcs.destruct_meshgl t

let alloc () =
  let finalise = Mem.finaliser C.Types.MeshGL.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.MeshGL.t) buf

let copy t =
  let buf, fresh = alloc () in
  let _ = C.Funcs.meshgl_copy buf t in
  fresh

let num_prop t = C.Funcs.meshgl_num_prop t
let num_vert t = C.Funcs.meshgl_num_vert t
let num_tri t = C.Funcs.meshgl_num_tri t
let properties_length t = C.Funcs.meshgl_vert_properties_length t
let tri_length t = C.Funcs.meshgl_tri_length t
let merge_length t = C.Funcs.meshgl_merge_length t
let tangent_length t = C.Funcs.meshgl_tangent_length t

let properties t =
  let len = size_to_int @@ properties_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in
  let verts = C.Funcs.meshgl_vert_properties buf t in
  List.init len Ctypes.(fun i -> !@(verts +@ i))

let triangles t =
  let len = size_to_int @@ tri_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof uint32_t)) in
  let tris = C.Funcs.meshgl_tri_verts buf t in
  List.init len Ctypes.(fun i -> Unsigned.UInt32.to_int !@(tris +@ i))

let halfedge_tangents t =
  let len = size_to_int @@ tangent_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in
  let tans = C.Funcs.meshgl_halfedge_tangent buf t in
  List.init len Ctypes.(fun i -> !@(tans +@ i))
