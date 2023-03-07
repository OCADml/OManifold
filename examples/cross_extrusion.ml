open OCADml
open OManifold

let () =
  let sq = CrossSection.square (v2 10. 10.) in
  let a = CrossSection.(rotate (Float.pi /. 4.) (translate (v2 4. 6.) sq))
  and b =
    let m = Affine2.(translate (v2 4. 6.) %> rotate (Float.pi /. 4.)) in
    CrossSection.affine m sq
  in
  Manifold.extrude ~height:5. a
  |> Manifold.add (Manifold.extrude ~height:5. b)
  |> Export.manifold "cross_section_transform.stl"
