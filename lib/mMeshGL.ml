open OCADml
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

let make ?normals ?tangents points triangles =
  let buf, t = alloc () in
  let len_ps = List.length points
  and len_tris = List.length triangles in
  let n_verts = size_of_int len_ps
  and n_tris = size_of_int len_tris in
  let n_props, ps =
    match normals with
    | None ->
      let ps = Ctypes.(CArray.make float (len_ps * 3)) in
      List.iteri
        (fun i n ->
          Ctypes.CArray.set ps (i * 3) (V3.x n);
          Ctypes.CArray.set ps ((i * 3) + 1) (V3.y n);
          Ctypes.CArray.set ps ((i * 3) + 2) (V3.z n) )
        points;
      size_of_int 3, ps
    | Some norms ->
      let ps = Ctypes.(CArray.make float (len_ps * 6)) in
      ( try
          Util.iter2i
            (fun i p n ->
              Ctypes.CArray.set ps (i * 6) (V3.x p);
              Ctypes.CArray.set ps ((i * 6) + 1) (V3.y p);
              Ctypes.CArray.set ps ((i * 6) + 2) (V3.z p);
              Ctypes.CArray.set ps ((i * 6) + 3) (V3.x n);
              Ctypes.CArray.set ps ((i * 6) + 4) (V3.y n);
              Ctypes.CArray.set ps ((i * 6) + 5) (V3.z n) )
            points
            norms
        with
        | Invalid_argument _ ->
          invalid_arg "If normals are provided, their length must be the same as points."
      );
      size_of_int 6, ps
  in
  let tris = Ctypes.(CArray.make uint32_t (len_tris * 3)) in
  List.iteri
    (fun i (a, b, c) ->
      Ctypes.CArray.set tris (i * 3) (Unsigned.UInt32.of_int a);
      Ctypes.CArray.set tris ((i * 3) + 1) (Unsigned.UInt32.of_int b);
      Ctypes.CArray.set tris ((i * 3) + 2) (Unsigned.UInt32.of_int c) )
    triangles;
  let ps = Ctypes.CArray.start ps
  and tris = Ctypes.CArray.start tris in
  let _ =
    match tangents with
    | None -> C.Funcs.meshgl buf ps n_verts n_props tris n_tris
    | Some tans ->
      let len = List.length tans in
      if len <> len_tris * 3
      then
        invalid_arg
          "If halfedge tangents are provided, their length must be exactly three times \
           the length of triangles."
      else (
        let ts = Ctypes.(CArray.make float (len * 4)) in
        List.iteri
          (fun i n ->
            Ctypes.CArray.set ts (i * 4) (Gg.V4.x n);
            Ctypes.CArray.set ts ((i * 4) + 1) (Gg.V4.y n);
            Ctypes.CArray.set ts ((i * 4) + 2) (Gg.V4.z n);
            Ctypes.CArray.set ts ((i * 4) + 3) (Gg.V4.w n) )
          tans;
        let ts = Ctypes.CArray.start ts in
        C.Funcs.meshgl_w_tangents buf ps n_verts n_props tris n_tris ts )
  in
  t

let num_prop t = C.Funcs.meshgl_num_prop t
let num_vert t = C.Funcs.meshgl_num_vert t
let num_tri t = C.Funcs.meshgl_num_tri t
let properties_length t = C.Funcs.meshgl_vert_properties_length t
let tri_length t = C.Funcs.meshgl_tri_length t
let merge_length t = C.Funcs.meshgl_merge_length t
let tangent_length t = C.Funcs.meshgl_tangent_length t
let run_index_length t = C.Funcs.meshgl_run_index_length t
let run_original_id_length t = C.Funcs.meshgl_run_original_id_length t
let run_transform_length t = C.Funcs.meshgl_run_transform_length t
let face_id_length t = C.Funcs.meshgl_face_id_length t

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

let run_index t =
  let len = size_to_int @@ run_index_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof uint32_t)) in
  let tans = C.Funcs.meshgl_run_index buf t in
  List.init len Ctypes.(fun i -> Unsigned.UInt32.to_int !@(tans +@ i))

let run_original_id t =
  let len = size_to_int @@ run_original_id_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof uint32_t)) in
  let tans = C.Funcs.meshgl_run_original_id buf t in
  List.init len Ctypes.(fun i -> Unsigned.UInt32.to_int !@(tans +@ i))

let run_transform t =
  let len = size_to_int @@ run_transform_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in
  let tans = C.Funcs.meshgl_run_transform buf t in
  List.init len Ctypes.(fun i -> !@(tans +@ i))

let face_id t =
  let len = size_to_int @@ face_id_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof uint32_t)) in
  let tans = C.Funcs.meshgl_face_id buf t in
  List.init len Ctypes.(fun i -> Unsigned.UInt32.to_int !@(tans +@ i))

let points t =
  let len = size_to_int @@ properties_length t
  and n = num_prop t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in
  let verts = C.Funcs.meshgl_vert_properties buf t in
  let f i =
    Ctypes.(v3 !@(verts +@ (i * n)) !@(verts +@ ((i * n) + 1)) !@(verts +@ ((i * n) + 2)))
  in
  List.init (num_vert t) f

let faces t =
  let len = size_to_int @@ tri_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof uint32_t)) in
  let tris = C.Funcs.meshgl_tri_verts buf t in
  let f i =
    Ctypes.(
      ( Unsigned.UInt32.to_int !@(tris +@ (i * 3))
      , Unsigned.UInt32.to_int !@(tris +@ ((i * 3) + 1))
      , Unsigned.UInt32.to_int !@(tris +@ ((i * 3) + 2)) ) )
  in
  List.init (len / 3) f

let of_mesh ?(rev = true) (m : Mesh.t) =
  (* OCADml follows OpenSCAD winding convention, opposite to Manifold *)
  let rev_tri (a, b, c) = c, b, a in
  if rev
  then make (Mesh.points m) (List.rev_map rev_tri (Mesh.faces m))
  else make (Mesh.points m) (Mesh.faces m)

let to_mesh t = Mesh.make ~points:(points t) ~faces:(faces t)
