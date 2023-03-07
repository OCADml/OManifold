open OCADml
open OManifold

let%test "uncollected" =
  let tet = Manifold.tetrahedron () in
  Gc.full_major ();
  let tet2 = Manifold.add tet (Manifold.tetrahedron ()) in
  Gc.full_major ();
  let f = Manifold.(add (add tet (tetrahedron ()))) in
  Gc.full_major ();
  let a = f (Manifold.add tet tet2) in
  Gc.full_major ();
  let b = f Manifold.(add (f a) (f (add (tetrahedron ()) tet))) in
  Gc.full_major ();
  let _ = Manifold.add (f b) a in
  true (* no segfaults due to early free / double free *)

let%test "box" =
  let a = v3 0. 0. 0.
  and b = v3 2. 2. 2. in
  let box = MBox.make a b in
  V3.(equal (MBox.min box) a && equal (MBox.max box) b)

let%test "warp" =
  let s = Manifold.sphere 10. in
  let _ = Manifold.warp (V3.translate (v3 1. 1. 1.)) s in
  true (* no failure in C *)

let%test "decompose_compose" =
  let spheres = Manifold.(add (sphere 2.) (ztrans 5. (sphere 2.))) in
  Gc.full_major ();
  let decomp = Manifold.decompose spheres in
  Gc.full_major ();
  let shifted = List.map (Manifold.xtrans 5.) decomp in
  Gc.full_major ();
  let composed = Manifold.compose @@ (spheres :: shifted) in
  Gc.full_major ();
  let decomp = Manifold.decompose composed in
  Gc.full_major ();
  List.length decomp = 4

let%test "rect_clipping" =
  let cs = CrossSection.square (v2 10. 10.) in
  let r = MRect.translate (v2 0. (-5.)) @@ CrossSection.bounds cs in
  CrossSection.(area cs /. 2. = area @@ rect_clip cs r)

let%test "cross_transform" =
  let sq = CrossSection.square (v2 10. 10.)
  and rot = Float.pi /. 4.
  and trans = v2 4. 6. in
  let a = CrossSection.(rotate rot (translate trans sq))
  and b = CrossSection.affine Affine2.(translate trans %> rotate rot) sq in
  CrossSection.(MRect.contains_rect (bounds a) (bounds b))
