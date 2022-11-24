open OCADml
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

let make ?normals ?tangents points triangles =
  let buf, t = alloc () in
  let len_ps = List.length points
  and len_tris = List.length triangles in
  let sz_ps = size_of_int len_ps
  and sz_tris = size_of_int len_tris in
  let normals =
    match normals with
    | None -> None
    | Some norms ->
      let len = List.length norms in
      if len <> len_ps
      then invalid_arg "If normals are provided, their length must be the same as points."
      else (
        let ns = Ctypes.CArray.make C.Types.Vec3.t len in
        List.iteri (fun i n -> Ctypes.CArray.set ns i (vec3_of_v3 n)) norms;
        Some (Ctypes.CArray.start ns) )
  and tangents =
    match tangents with
    | None -> None
    | Some tans ->
      let len = List.length tans in
      if len <> len_ps
      then
        invalid_arg
          "If halfedge tangents are provided, their length must be exactly three times \
           the length of triangles."
      else (
        let ts = Ctypes.CArray.make C.Types.Vec4.t len in
        List.iteri (fun i t -> Ctypes.CArray.set ts i (vec4_of_v4 t)) tans;
        Some (Ctypes.CArray.start ts) )
  in
  let ps = Ctypes.CArray.make C.Types.Vec3.t len_ps
  and tris = Ctypes.CArray.make C.Types.IVec3.t len_tris in
  List.iteri (fun i p -> Ctypes.CArray.set ps i (vec3_of_v3 p)) points;
  List.iteri (fun i t -> Ctypes.CArray.set tris i (ivec3_of_tup t)) triangles;
  let ps = Ctypes.CArray.start ps
  and tris = Ctypes.CArray.start tris in
  let _ =
    match normals, tangents with
    | None, None -> C.Funcs.mesh buf ps sz_ps tris sz_tris
    | Some ns, None -> C.Funcs.mesh_w_normals buf ps sz_ps tris sz_tris ns
    | None, Some ts -> C.Funcs.mesh_w_tangents buf ps sz_ps tris sz_tris ts
    | Some ns, Some ts -> C.Funcs.mesh_w_normals_tangents buf ps sz_ps tris sz_tris ns ts
  in
  t

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

let of_mesh ?normals ?tangents (m : Mesh.t) =
  let[@warning "-partial-match"] to_tri [ a; b; c ] = a, b, c
  and m = Mesh.triangulate m in
  make ?normals ?tangents (Mesh.points m) (List.map to_tri (Mesh.faces m))

let to_mesh t = Mesh.make ~points:(points t) ~faces:(faces t)
