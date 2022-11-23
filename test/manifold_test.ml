open OCADml
open OManifold

let no_error m =
  match Manifold.status m with
  | NoError -> true
  | _ -> false

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
  no_error @@ Manifold.add (f b) a

let%test "box" =
  let a = v3 0. 0. 0.
  and b = v3 2. 2. 2. in
  let box = Box.make a b in
  V3.(equal (Box.min box) a && equal (Box.max box) b)

let%test "warp" =
  let s = Manifold.sphere 10. in
  no_error @@ Manifold.warp (V3.translate (v3 1. 1. 1.)) s
