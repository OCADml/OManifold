open OCADml
open OManifold

let control =
  V3.[ v 5. 5. 12.; v 0. 20. 20.; v 30. 30. 0.; v 50. 20. 5.; v 35. (-10.) 15. ]

let path = Bezier3.curve ~fn:30 @@ Bezier3.of_path control

let poly =
  let holes =
    let s = Path2.circle ~fn:90 2.
    and d = 1.9 in
    Path2.[ translate (v2 (-.d) (-.d)) s; translate (v2 d d) s ]
  and outer =
    Path2.square ~center:true (v2 10. 10.)
    |> Path2.Round.(flat ~corner:(chamf (`Width 2.)))
    |> Path2.roundover
  in
  Poly2.make ~holes outer

let caps =
  Mesh.Cap.(
    capped
      ~bot:(round ~holes:`Same @@ chamf ~height:(-1.2) ~angle:(Float.pi /. 8.) ())
      ~top:(round @@ circ (`Radius 0.5)))

let () =
  Mesh.path_extrude ~path ~caps poly
  |> Manifold.of_mesh
  |> Export.manifold "rounded_polyhole_sweep.stl"

let rounded_path =
  Path3.(
    roundover ~fn:32
    @@ Round.flat
         ~closed:true
         ~corner:(Round.circ (`Radius 10.))
         (v3 (-25.) 25. 0. :: Path3.square (v2 50. 50.)))

let () =
  let loop = Manifold.of_mesh @@ Mesh.path_extrude ~caps:`Looped ~path:rounded_path poly
  and cut =
    Manifold.cylinder ~fn:50 ~center:true ~height:11. 5.
    |> Manifold.scale (v3 1.2 1. 1.)
    |> Manifold.translate (v3 20. (-4.) 0.)
  in
  Export.manifold "chamfered_loop.stl" @@ Manifold.difference loop [ cut ]
