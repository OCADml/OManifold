open OCADml
open OManifold

let () =
  let s = Manifold.sphere ~fn:16 2. in
  Manifold.(hull_exn [ s; xtrans 10. s; ytrans 10. s; translate (v3 10. 10. 0.) s ])
  |> Manifold.to_mmesh
  |> Export.mmesh "hulled.stl"
