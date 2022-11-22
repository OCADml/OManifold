open OCADml
open Omanifold

let%test "uncollected" =
  let tet = Manifold.tetrahedron () in
  Gc.full_major ();
  let tet2 = Manifold.union tet (Manifold.tetrahedron ()) in
  Gc.full_major ();
  let f = Manifold.(union (union tet (tetrahedron ()))) in
  Gc.full_major ();
  let a = f (Manifold.union tet tet2) in
  Gc.full_major ();
  let b = f Manifold.(union (f a) (f (union (tetrahedron ()) tet))) in
  Gc.full_major ();
  let c = Manifold.union (f b) a in
  ignore c;
  true

let%test "box" =
  let a = v3 0. 0. 0.
  and b = v3 2. 2. 2. in
  let box = Box.make a b in
  V3.(equal (Box.min box) a && equal (Box.max box) b)

let%test "warp" =
  let s = Manifold.sphere 10. in
  match Manifold.status (Manifold.warp (V3.translate (v3 1. 1. 1.)) s) with
  | NoError -> true
  | _ -> false

let%test "sdf" =
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
  (* let f = Sdf.sphere rad *)
  and box = Box.make bb_max (V3.neg bb_max) in
  let mesh = Sdf.to_mmesh ~box f in
  Export.export_mesh "sdf_mesh.stl" mesh;
  ignore (f, box);
  ignore mesh;
  true
