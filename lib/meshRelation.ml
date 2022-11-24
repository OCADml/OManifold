open OCADml
open Conv

module BaryRef = struct
  module BR = C.Types.BaryRef

  type t =
    { mesh_id : int
    ; original_id : int
    ; tri : int
    ; vert_bary : int * int * int
    }

  let of_struct s =
    let mesh_id = Ctypes.getf s BR.mesh_id
    and original_id = Ctypes.getf s BR.original_id
    and tri = Ctypes.getf s BR.tri
    and vert_bary = Conv.ivec3_to_tup (Ctypes.getf s BR.vert_bary) in
    { mesh_id; original_id; tri; vert_bary }

  let to_struct t =
    let s = Ctypes.make BR.t in
    Ctypes.setf s BR.mesh_id t.mesh_id;
    Ctypes.setf s BR.original_id t.original_id;
    Ctypes.setf s BR.tri t.tri;
    Ctypes.setf s BR.vert_bary (Conv.ivec3_of_tup t.vert_bary);
    s
end

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
