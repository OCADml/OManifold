open OCADml
open OManifold

let () =
  let bb_max = v3 10. 10. 10. in
  let f =
    Sdf3.cylinder ~round:2. ~height:12. 4.
    |> Sdf3.elongate (v3 0. 0. 4.)
    |> Sdf3.union
         ~smooth:1.
         Sdf2.(extrude ~height:8. @@ rounded_box ~tl:4. ~br:4. (v2 16. 16.))
    |> Sdf3.onion 1.
    |> Fun.flip Sdf3.difference Sdf3.(ztrans 5. @@ cube (v3 18. 18. 10.))
  and box = Box.make bb_max (V3.neg bb_max) in
  let mesh = Sdf3.to_mmesh ~edge_length:1. ~box f in
  match Manifold.of_mmesh mesh with
  | Ok m -> Export.mmesh "sdf_mesh.stl" @@ Manifold.to_mmesh m
  | Error e -> failwith (Printf.sprintf "Sdf mesh is non-manifold: %s" e)

let () =
  let bb_max = v3 10. 10. 10. in
  let f =
    Sdf2.rhombus ~round:1. (v2 15. 6.)
    |> Sdf2.onion 1.
    |> Sdf2.extrude ~height:3.
    |> Sdf3.translate (v3 5. 0. 0.)
    (* Sdf2.rhombus (v2 6. 3.) |> Sdf2.round 1. |> Sdf2.onion 1. |> Sdf2.revolve *)
  and box = Box.make bb_max (V3.neg bb_max) in
  let mesh = Sdf3.to_mmesh ~edge_length:1. ~box f in
  match Manifold.of_mmesh mesh with
  | Ok m -> Export.mmesh "sdf_extrude.stl" @@ Manifold.to_mmesh m
  | Error e -> failwith (Printf.sprintf "Sdf extrusion is non-manifold: %s" e)
