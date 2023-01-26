open Conv

module TriRef = struct
  module TR = C.Types.TriRef

  type t =
    { mesh_id : int
    ; original_id : int
    ; tri : int
    }

  let of_struct s =
    let mesh_id = Ctypes.getf s TR.mesh_id
    and original_id = Ctypes.getf s TR.original_id
    and tri = Ctypes.getf s TR.tri in
    { mesh_id; original_id; tri }

  let to_struct t =
    let s = Ctypes.make TR.t in
    Ctypes.setf s TR.mesh_id t.mesh_id;
    Ctypes.setf s TR.original_id t.original_id;
    Ctypes.setf s TR.tri t.tri;
    s
end

type ptr = C.Types.MeshRelation.t Ctypes_static.ptr
type t = { tri_ref : TriRef.t list }

let size = C.Funcs.mesh_relation_size () |> size_to_int
let destruct t = C.Funcs.destruct_mesh_relation t

let alloc () =
  let finalise = Mem.finaliser C.Types.MeshRelation.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.MeshRelation.t) buf)

let tri_ref_length t = C.Funcs.mesh_relation_tri_ref_length t

let tri_ref t =
  let len = size_to_int @@ tri_ref_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof C.Types.TriRef.t)) in
  let tri = C.Funcs.mesh_relation_tri_ref buf t in
  List.init len Ctypes.(fun i -> TriRef.of_struct !@(tri +@ i))

let of_ptr p = { tri_ref = tri_ref p }
