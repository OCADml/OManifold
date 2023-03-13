let get_circular_segments r = C.Funcs.manifold_get_circular_segments r
let set_circular_segments fn = C.Funcs.manifold_set_circular_segments fn

let set_min_circular_angle fa =
  C.Funcs.manifold_set_min_circular_angle (OCADml.Math.deg_of_rad fa)

let set_min_circular_edge_length fs = C.Funcs.manifold_set_min_circular_edge_length fs
