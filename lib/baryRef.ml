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
