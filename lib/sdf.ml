open OCADml

type t = v3 -> float

let sphere r p = r -. V3.norm p

let cube ?(radius = 0.) (dims : v3) (p : v3) =
  let q = Float.(v3 (abs p.x -. dims.x) (abs p.y -. dims.y) (abs p.z -. dims.z)) in
  let len = Float.(V3.norm (v3 (max q.x 0.) (max q.y 0.) (max q.z 0.))) in
  Float.neg @@ (len +. Float.(min (max q.x (max q.y q.z)) 0.) -. radius)

let torus (v : v2) (p : v3) =
  let q = v2 (V2.norm (v2 p.x p.z) -. v.x) p.y in
  Float.neg @@ (V2.norm q -. v.y)

let level_set ?(level = 0.) ?(edge_length = 0.5) ~box sdf =
  let sdf x y z = sdf (v3 x y z) in
  let buf, mesh = MMesh.alloc ()
  and f =
    Ctypes.(coerce (Foreign.funptr C.Funcs.sdf_t) (static_funptr C.Funcs.sdf_t) sdf)
  in
  let _ = C.Funcs.level_set buf f box edge_length level in
  mesh
