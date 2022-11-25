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
  Export.mmesh "sphere.glb" mesh

let () =
  (* let rad = 15. in *)
  (* let bb_max = v3 (rad *. 2.) (rad *. 2.) (rad *. 2.) in *)
  let bb_max = v3 10. 10. 10. in
  (* let f = Sdf3.cube ~radius:1. (v3 5. 5. 5.) *)
  (* let f = Sdf3.torus (v2 5. 1.) *)
  let f =
    Sdf3.cylinder ~height:5. 4.
    |> Sdf3.round 2.
    |> Sdf3.quaternion (Quaternion.make (v3 1. 0. 0.) (Float.pi /. 2.))
    |> Sdf3.elongate (v3 0. 0. 4.)
    |> Sdf3.union ~smooth:1. (Sdf3.scale 2.5 @@ Sdf3.cube (v3 4. 4. 2.))
  (* let f = Sdf3.sphere 4.5 *)
  and box = Box.make bb_max (V3.neg bb_max) in
  let mesh = Sdf3.to_mmesh ~edge_length:1. ~box f in
  let man = Manifold.of_mmesh mesh in
  print_endline Manifold.(Status.to_string (status man));
  let mesh = Manifold.to_mmesh man in
  Export.mmesh "sdf_mesh.glb" mesh

let () =
  let bb_max = v3 10. 10. 10. in
  let f =
    Sdf2.rhombus (v2 6. 3.) |> Sdf2.round 1. |> Sdf2.onion 1. |> Sdf2.extrude ~height:3.
  and box = Box.make bb_max (V3.neg bb_max) in
  let mesh = Sdf3.to_mmesh ~edge_length:1. ~box f in
  let man = Manifold.of_mmesh mesh in
  print_endline Manifold.(Status.to_string (status man));
  let mesh = Manifold.to_mmesh man in
  Export.mmesh "sdf_extrude.glb" mesh
