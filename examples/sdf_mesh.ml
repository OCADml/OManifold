open OCADml
open OManifold

let () =
  let s = Manifold.sphere ~fn:64 10. in
  (* NOTE: not doing anything, not sure if I misunderstand *)
  Manifold.set_circular_segments s 128;
  print_endline (Manifold.circular_segments s 10. |> Int.to_string);
  let s = Manifold.refine 2 s in
  let s = Manifold.copy s in
  let mesh = Manifold.to_mmesh s in
  Export.export_mesh "sphere.stl" mesh

let () =
  let s = Manifold.sphere 5. in
  Manifold.(hull [ s; xtrans 10. s; ytrans 10. s; translate (v3 10. 10. 0.) s ])
  |> Manifold.to_mmesh
  |> Export.export_mesh "hulled.stl"

let () =
  (* let rad = 15. in *)
  (* let bb_max = v3 (rad *. 2.) (rad *. 2.) (rad *. 2.) in *)
  let bb_max = v3 10. 10. 10. in
  (* let f = Sdf.cube ~radius:1. (v3 5. 5. 5.) *)
  (* let f = Sdf.torus (v2 5. 1.) *)
  let f =
    Sdf.cylinder ~height:5. 4.
    |> Sdf.round 2.
    |> Sdf.quaternion (Quaternion.make (v3 1. 0. 0.) (Float.pi /. 2.))
    |> Sdf.elongate (v3 0. 0. 4.)
    |> Sdf.union ~smooth:1. (Sdf.scale 2.5 @@ Sdf.cube (v3 4. 4. 2.))
  (* let f = Sdf.sphere 4.5 *)
  and box = Box.make bb_max (V3.neg bb_max) in
  let mesh = Sdf.to_mmesh ~edge_length:0.25 ~box f in
  let man = Manifold.of_mmesh mesh in
  print_endline Manifold.(Status.to_string (status man));
  let mesh = Manifold.to_mmesh man in
  Export.export_mesh "sdf_mesh.stl" mesh
