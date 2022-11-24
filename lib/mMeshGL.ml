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

let vert_length t = C.Funcs.meshgl_vert_length t
let tri_length t = C.Funcs.meshgl_tri_length t
let normal_length t = C.Funcs.meshgl_normal_length t
let tangent_length t = C.Funcs.meshgl_tangent_length t

(* let points t = *)
(*   let len = size_to_int @@ vert_length t in *)
(*   let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in *)
(*   let verts = C.Funcs.meshgl_vert_pos buf t in *)
(*   List.init len Ctypes.(fun i -> !@(verts +@ i)) *)

(* let triangles t = *)
(*   let len = size_to_int @@ tri_length t in *)
(*   let buf = Mem.allocate_buf (len * Ctypes.(sizeof uint32_t)) in *)
(*   let tris = C.Funcs.meshgl_tri_verts buf t in *)
(*   List.init len Ctypes.(fun i -> Unsigned.UInt32.to_int !@(tris +@ i)) *)

(* let normals t = *)
(*   let len = size_to_int @@ normal_length t in *)
(*   let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in *)
(*   let norms = C.Funcs.meshgl_vert_normal buf t in *)
(*   List.init len Ctypes.(fun i -> !@(norms +@ i)) *)

(* let tangents t = *)
(*   let len = size_to_int @@ tangent_length t in *)
(*   let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in *)
(*   let tans = C.Funcs.meshgl_halfedge_tangent buf t in *)
(*   List.init len Ctypes.(fun i -> !@(tans +@ i)) *)

(* TODO: maybe should return CArray instead since these output arrangements
are meant for passing along to a graphics library anyway. However, they aren't
    necessarily ready to use depending on the library anyway. For example,
    raylib doesn't want indices into points, it expects the vertices to be
    grouped by their triangles (which I don't think these are). So maybe stick
    with lists to keep Ctypes out of the high-level interface? *)
let points t =
  let len = size_to_int @@ vert_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in
  let verts = C.Funcs.meshgl_vert_pos buf t in
  Ctypes.CArray.from_ptr verts len

let triangles t =
  let len = size_to_int @@ tri_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof uint32_t)) in
  let tris = C.Funcs.meshgl_tri_verts buf t in
  Ctypes.(CArray.(map int Unsigned.UInt32.to_int @@ from_ptr tris len))

let normals t =
  let len = size_to_int @@ normal_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in
  let norms = C.Funcs.meshgl_vert_normal buf t in
  Ctypes.CArray.from_ptr norms len

let tangents t =
  let len = size_to_int @@ tangent_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in
  let tans = C.Funcs.meshgl_halfedge_tangent buf t in
  Ctypes.CArray.from_ptr tans len
