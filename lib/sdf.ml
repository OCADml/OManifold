open OCADml

type t = v3 -> float

let level_set ?(level = 0.) ?(edge_length = 0.5) ~box sdf =
  let sdf x y z = sdf (v3 x y z) in
  let buf, mesh = MMesh.alloc ()
  and f =
    (* sdf *)
    Ctypes.(coerce (Foreign.funptr C.Funcs.sdf_t) (static_funptr C.Funcs.sdf_t) sdf)
    (* C.Funcs.Sdf_dyn.of_fun sdf *)
  in
  let _ = C.Funcs.manifold_level_set buf f box edge_length level in
  (* C.Funcs.Sdf_dyn.free f; *)
  mesh
