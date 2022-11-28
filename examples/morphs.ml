open OCADml
open OManifold

let () =
  let path =
    let control =
      V3.[ v 0. 0. 2.; v 0. 20. 20.; v 40. 20. 10.; v 30. 0. 10. ]
      |> Path3.quaternion (Quaternion.make (v3 1. 1. 0.) (Float.pi /. -5.))
    in
    Bezier3.curve ~fn:60 @@ Bezier3.of_path ~size:(`Flat (`Rel 0.3)) control
  and caps =
    Mesh.Cap.{ bot = round @@ circ (`Radius 0.5); top = round @@ circ (`Radius 0.5) }
  and a = Poly2.ring ~fn:5 ~thickness:(v2 2.5 2.5) (v2 6. 6.)
  and b = Poly2.ring ~fn:80 ~thickness:(v2 2. 2.) (v2 4. 4.) in
  Mesh.path_morph ~refine:2 ~caps ~path ~outer_map:`Tangent a b
  |> Manifold.of_mesh_exn
  |> Export.manifold "tangent_morph_sweep.stl"

let man =
  let top =
    Mesh.morph
      ~refine:2
      ~ez:(v2 0.42 0., v2 1. 1.)
      ~slices:60
      ~outer_map:`Tangent
      ~height:3.
      (Poly2.ring ~fn:5 ~thickness:(v2 0.5 0.5) (v2 4. 4.))
      (Poly2.ring ~fn:80 ~thickness:(v2 0.2 0.2) (v2 1. 1.))
    |> Manifold.of_mesh_exn
  in
  Manifold.(add (ztrans 2. top) (ztrans (-2.) @@ xrot Float.pi top))

let () = Export.manifold "eased_morph.stl" man
