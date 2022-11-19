module Descriptions (F : Ctypes.FOREIGN) = struct
  open! Ctypes
  open! F
  open! Manifold_c_types

  let manifold_size = foreign "manifold_manifold_size" (void @-> returning size_t)

  let simple_polygon_size =
    foreign "manifold_simple_polygon_size" (void @-> returning size_t)

  let polygons_size = foreign "manifold_polygons_size" (void @-> returning size_t)

  let manifold_pair_size =
    foreign "manifold_manifold_pair_size" (void @-> returning size_t)

  let mesh_size = foreign "manifold_mesh_size" (void @-> returning size_t)
  let meshgl_size = foreign "manifold_meshgl_size" (void @-> returning size_t)
  let box_size = foreign "manifold_box_size" (void @-> returning size_t)
  let curvature_size = foreign "manifold_curvature_size" (void @-> returning size_t)

  let mesh_relation_size =
    foreign "manifold_mesh_relation_size" (void @-> returning size_t)

  let material_size = foreign "manifold_material_size" (void @-> returning size_t)

  let export_options_size =
    foreign "manifold_export_options_size" (void @-> returning size_t)

  let delete_manifold =
    foreign "manifold_delete_manifold" (ptr Manifold.t @-> returning void)

  let delete_simple_polygon =
    foreign "manifold_delete_simple_polygon" (ptr SimplePolygon.t @-> returning void)

  let delete_polygons =
    foreign "manifold_delete_polygons" (ptr Polygons.t @-> returning void)

  let delete_mesh = foreign "manifold_delete_mesh" (ptr Mesh.t @-> returning void)
  let delete_meshgl = foreign "manifold_delete_meshgl" (ptr MeshGL.t @-> returning void)
  let delete_box = foreign "manifold_delete_box" (ptr Box.t @-> returning void)

  let delete_curvature =
    foreign "manifold_delete_curvature" (ptr Curvature.t @-> returning void)

  let delete_mesh_relation =
    foreign "manifold_delete_mesh_relation" (ptr MeshRelation.t @-> returning void)

  let delete_material =
    foreign "manifold_delete_material" (ptr Material.t @-> returning void)

  let delete_export_options =
    foreign "manifold_delete_export_options" (ptr ExportOptions.t @-> returning void)
end
