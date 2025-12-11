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

let%test "cross_transform" =
  let sq = Cross.square (v2 10. 10.)
  and rot = Float.pi /. 4.
  and trans = v2 4. 6. in
  let a = Cross.(rotate rot (translate trans sq))
  and b = Cross.affine Affine2.(translate trans %> rotate rot) sq in
  Cross.(Box2.subset (bounds b) (bounds a))
