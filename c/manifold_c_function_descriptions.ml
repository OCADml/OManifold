module Functions (F : Ctypes.FOREIGN) = struct
  open Ctypes
  open F
  open Manifold_c_types

  (* Polygons *)

  let simple_polygon =
    foreign
      "manifold_simple_polygon"
      (ptr void @-> ptr Vec2.t @-> size_t @-> returning (ptr SimplePolygon.t))

  let polygons =
    foreign
      "manifold_polygons"
      (ptr void @-> ptr (ptr SimplePolygon.t) @-> size_t @-> returning (ptr Polygons.t))

  (* Mesh Construction *)

  let meshgl =
    foreign
      "manifold_meshgl"
      ( ptr void
      @-> ptr float
      @-> size_t
      @-> size_t
      @-> ptr uint32_t
      @-> size_t
      @-> returning (ptr MeshGL.t) )

  let meshgl_w_tangents =
    foreign
      "manifold_meshgl_w_tangents"
      ( ptr void
      @-> ptr float
      @-> size_t
      @-> size_t
      @-> ptr uint32_t
      @-> size_t
      @-> ptr float
      @-> returning (ptr MeshGL.t) )

  let manifold_get_meshgl =
    foreign
      "manifold_get_meshgl"
      (ptr void @-> ptr Manifold.t @-> returning (ptr MeshGL.t))

  let meshgl_copy =
    foreign "manifold_meshgl_copy" (ptr void @-> ptr MeshGL.t @-> returning (ptr MeshGL.t))

  (* Mesh Info  *)

  let meshgl_num_prop = foreign "manifold_meshgl_num_prop" (ptr MeshGL.t @-> returning int)
  let meshgl_num_vert = foreign "manifold_meshgl_num_vert" (ptr MeshGL.t @-> returning int)
  let meshgl_num_tri = foreign "manifold_meshgl_num_tri" (ptr MeshGL.t @-> returning int)

  let meshgl_vert_properties_length =
    foreign "manifold_meshgl_vert_properties_length" (ptr MeshGL.t @-> returning size_t)

  let meshgl_tri_length =
    foreign "manifold_meshgl_tri_length" (ptr MeshGL.t @-> returning size_t)

  let meshgl_merge_length =
    foreign "manifold_meshgl_merge_length" (ptr MeshGL.t @-> returning size_t)

  let meshgl_run_index_length =
    foreign "manifold_meshgl_run_index_length" (ptr MeshGL.t @-> returning size_t)

  let meshgl_run_original_id_length =
    foreign "manifold_meshgl_run_original_id_length" (ptr MeshGL.t @-> returning size_t)

  let meshgl_run_transform_length =
    foreign "manifold_meshgl_run_transform_length" (ptr MeshGL.t @-> returning size_t)

  let meshgl_face_id_length =
    foreign "manifold_meshgl_face_id_length" (ptr MeshGL.t @-> returning size_t)

  let meshgl_tangent_length =
    foreign "manifold_meshgl_tangent_length" (ptr MeshGL.t @-> returning size_t)

  let meshgl_vert_properties =
    foreign
      "manifold_meshgl_vert_properties"
      (ptr void @-> ptr MeshGL.t @-> returning (ptr float))

  let meshgl_tri_verts =
    foreign
      "manifold_meshgl_tri_verts"
      (ptr void @-> ptr MeshGL.t @-> returning (ptr uint32_t))

  let meshgl_merge_from_vert =
    foreign
      "manifold_meshgl_merge_from_vert"
      (ptr void @-> ptr MeshGL.t @-> returning (ptr uint32_t))

  let meshgl_merge_to_vert =
    foreign
      "manifold_meshgl_merge_to_vert"
      (ptr void @-> ptr MeshGL.t @-> returning (ptr uint32_t))

  let meshgl_run_index =
    foreign
      "manifold_meshgl_run_index"
      (ptr void @-> ptr MeshGL.t @-> returning (ptr uint32_t))

  let meshgl_run_original_id =
    foreign
      "manifold_meshgl_run_original_id"
      (ptr void @-> ptr MeshGL.t @-> returning (ptr uint32_t))

  let meshgl_run_transform =
    foreign
      "manifold_meshgl_run_transform"
      (ptr void @-> ptr MeshGL.t @-> returning (ptr float))

  let meshgl_face_id =
    foreign
      "manifold_meshgl_face_id"
      (ptr void @-> ptr MeshGL.t @-> returning (ptr uint32_t))

  let meshgl_halfedge_tangent =
    foreign
      "manifold_meshgl_halfedge_tangent"
      (ptr void @-> ptr MeshGL.t @-> returning (ptr float))

  (* Manifold Shapes / Constructors *)

  let manifold_empty = foreign "manifold_empty" (ptr void @-> returning (ptr Manifold.t))

  let manifold_copy =
    foreign "manifold_copy" (ptr void @-> ptr Manifold.t @-> returning (ptr Manifold.t))

  let manifold_tetrahedron =
    foreign "manifold_tetrahedron" (ptr void @-> returning (ptr Manifold.t))

  let manifold_cube =
    foreign
      "manifold_cube"
      (ptr void @-> float @-> float @-> float @-> int @-> returning (ptr Manifold.t))

  let manifold_cylinder =
    foreign
      "manifold_cylinder"
      ( ptr void
      @-> float
      @-> float
      @-> float
      @-> int
      @-> int
      @-> returning (ptr Manifold.t) )

  let manifold_sphere =
    foreign "manifold_sphere" (ptr void @-> float @-> int @-> returning (ptr Manifold.t))

  let manifold_of_meshgl =
    foreign "manifold_of_meshgl" (ptr void @-> ptr MeshGL.t @-> returning (ptr Manifold.t))

  let manifold_smooth =
    foreign
      "manifold_smooth"
      ( ptr void
      @-> ptr MeshGL.t
      @-> ptr int
      @-> ptr float
      @-> size_t
      @-> returning (ptr Manifold.t) )

  let manifold_extrude =
    foreign
      "manifold_extrude"
      ( ptr void
      @-> ptr Polygons.t
      @-> float
      @-> int
      @-> float
      @-> float
      @-> float
      @-> returning (ptr Manifold.t) )

  let manifold_revolve =
    foreign
      "manifold_revolve"
      (ptr void @-> ptr Polygons.t @-> int @-> returning (ptr Manifold.t))

  let manifold_compose =
    foreign
      "manifold_compose"
      (ptr void @-> ptr (ptr Manifold.t) @-> size_t @-> returning (ptr Manifold.t))

  let manifold_get_components =
    foreign
      "manifold_get_components"
      (ptr void @-> ptr Manifold.t @-> returning (ptr Components.t))

  let manifold_components_length =
    foreign "manifold_components_length" (ptr Components.t @-> returning size_t)

  let manifold_decompose =
    foreign
      "manifold_decompose"
      ( ptr (ptr void)
      @-> ptr Manifold.t
      @-> ptr Components.t
      @-> returning (ptr (ptr Manifold.t)) )

  let manifold_as_original =
    foreign
      "manifold_as_original"
      (ptr void @-> ptr Manifold.t @-> returning (ptr Manifold.t))

  (* Booleans *)

  let manifold_union =
    foreign
      "manifold_union"
      (ptr void @-> ptr Manifold.t @-> ptr Manifold.t @-> returning (ptr Manifold.t))

  let manifold_difference =
    foreign
      "manifold_difference"
      (ptr void @-> ptr Manifold.t @-> ptr Manifold.t @-> returning (ptr Manifold.t))

  let manifold_intersection =
    foreign
      "manifold_intersection"
      (ptr void @-> ptr Manifold.t @-> ptr Manifold.t @-> returning (ptr Manifold.t))

  let manifold_split =
    foreign
      "manifold_split"
      ( ptr void
      @-> ptr void
      @-> ptr Manifold.t
      @-> ptr Manifold.t
      @-> returning ManifoldPair.t )

  let manifold_split_by_plane =
    foreign
      "manifold_split_by_plane"
      ( ptr void
      @-> ptr void
      @-> ptr Manifold.t
      @-> float
      @-> float
      @-> float
      @-> float
      @-> returning ManifoldPair.t )

  let manifold_trim_by_plane =
    foreign
      "manifold_trim_by_plane"
      ( ptr void
      @-> ptr Manifold.t
      @-> float
      @-> float
      @-> float
      @-> float
      @-> returning (ptr Manifold.t) )

  (* Transformations *)

  let manifold_translate =
    foreign
      "manifold_translate"
      ( ptr void
      @-> ptr Manifold.t
      @-> float
      @-> float
      @-> float
      @-> returning (ptr Manifold.t) )

  let manifold_rotate =
    foreign
      "manifold_rotate"
      ( ptr void
      @-> ptr Manifold.t
      @-> float
      @-> float
      @-> float
      @-> returning (ptr Manifold.t) )

  let manifold_scale =
    foreign
      "manifold_scale"
      ( ptr void
      @-> ptr Manifold.t
      @-> float
      @-> float
      @-> float
      @-> returning (ptr Manifold.t) )

  let manifold_transform =
    foreign
      "manifold_transform"
      ( ptr void
      @-> ptr Manifold.t
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> returning (ptr Manifold.t) )

  let warp_t = Ctypes.(float @-> float @-> float @-> returning Vec3.t)

  let manifold_warp =
    foreign
      "manifold_warp"
      (ptr void @-> ptr Manifold.t @-> static_funptr warp_t @-> returning (ptr Manifold.t))

  let manifold_refine =
    foreign
      "manifold_refine"
      (ptr void @-> ptr Manifold.t @-> int @-> returning (ptr Manifold.t))

  (* Manifold Mutation *)

  let manifold_set_min_circular_angle =
    foreign "manifold_set_min_circular_angle" (float @-> returning void)

  let manifold_set_min_circular_edge_length =
    foreign "manifold_set_min_circular_edge_length" (float @-> returning void)

  let manifold_set_circular_segments =
    foreign "manifold_set_circular_segments" (int @-> returning void)

  (* Manifold Info *)

  let manifold_is_empty = foreign "manifold_is_empty" (ptr Manifold.t @-> returning int)
  let manifold_status = foreign "manifold_status" (ptr Manifold.t @-> returning Status.t)
  let manifold_num_vert = foreign "manifold_num_vert" (ptr Manifold.t @-> returning int)
  let manifold_num_edge = foreign "manifold_num_edge" (ptr Manifold.t @-> returning int)
  let manifold_num_tri = foreign "manifold_num_tri" (ptr Manifold.t @-> returning int)

  let manifold_bounding_box =
    foreign "manifold_bounding_box" (ptr void @-> ptr Manifold.t @-> returning (ptr Box.t))

  let manifold_precision =
    foreign "manifold_precision" (ptr Manifold.t @-> returning float)

  let manifold_genus = foreign "manifold_genus" (ptr Manifold.t @-> returning int)

  let manifold_get_properties =
    foreign "manifold_get_properties" (ptr Manifold.t @-> returning Properties.t)

  let manifold_get_curvature =
    foreign
      "manifold_get_curvature"
      (ptr void @-> ptr Manifold.t @-> returning (ptr Curvature.t))

  let curvature_bounds =
    foreign "manifold_curvature_bounds" (ptr Curvature.t @-> returning CurvatureBounds.t)

  let curvature_vert_length =
    foreign "manifold_curvature_vert_length" (ptr Curvature.t @-> returning size_t)

  let curvature_vert_mean =
    foreign
      "manifold_curvature_vert_mean"
      (ptr void @-> ptr Curvature.t @-> returning (ptr float))

  let curvature_vert_gaussian =
    foreign
      "manifold_curvature_vert_gaussian"
      (ptr void @-> ptr Curvature.t @-> returning (ptr float))

  let manifold_get_circular_segments =
    foreign "manifold_get_circular_segments" (float @-> returning int)

  let manifold_original_id =
    foreign "manifold_original_id" (ptr Manifold.t @-> returning int)

  (* Bounding Box *)

  let box =
    foreign
      "manifold_box"
      ( ptr void
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> returning (ptr Box.t) )

  let box_min = foreign "manifold_box_min" (ptr Box.t @-> returning Vec3.t)
  let box_max = foreign "manifold_box_max" (ptr Box.t @-> returning Vec3.t)
  let box_dimensions = foreign "manifold_box_dimensions" (ptr Box.t @-> returning Vec3.t)
  let box_center = foreign "manifold_box_center" (ptr Box.t @-> returning Vec3.t)
  let box_scale = foreign "manifold_box_scale" (ptr Box.t @-> returning float)

  let box_contains_pt =
    foreign
      "manifold_box_contains_pt"
      (ptr Box.t @-> float @-> float @-> float @-> returning int)

  let box_contains_box =
    foreign "manifold_box_contains_box" (ptr Box.t @-> ptr Box.t @-> returning int)

  let box_include_pt =
    foreign
      "manifold_box_include_pt"
      (ptr Box.t @-> float @-> float @-> float @-> returning void)

  let box_union =
    foreign
      "manifold_box_union"
      (ptr void @-> ptr Box.t @-> ptr Box.t @-> returning (ptr Box.t))

  let box_transform =
    foreign
      "manifold_box_transform"
      ( ptr void
      @-> ptr Box.t
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> float
      @-> returning (ptr Box.t) )

  let box_translate =
    foreign
      "manifold_box_translate"
      (ptr void @-> ptr Box.t @-> float @-> float @-> float @-> returning (ptr Box.t))

  let box_mul =
    foreign
      "manifold_box_mul"
      (ptr void @-> ptr Box.t @-> float @-> float @-> float @-> returning (ptr Box.t))

  let box_does_overlap_pt =
    foreign
      "manifold_box_does_overlap_pt"
      (ptr Box.t @-> float @-> float @-> float @-> returning int)

  let box_does_overlap_box =
    foreign "manifold_box_does_overlap_box" (ptr Box.t @-> ptr Box.t @-> returning int)

  let box_is_finite = foreign "manifold_box_is_finite" (ptr Box.t @-> returning int)

  (* SDF *)

  let sdf_t = Ctypes.(float @-> float @-> float @-> returning float)

  let level_set_seq =
    foreign
      "manifold_level_set_seq"
      ( ptr void
      @-> static_funptr sdf_t
      @-> ptr Box.t
      @-> float
      @-> float
      @-> returning (ptr MeshGL.t) )

  (* Export *)
  let material = foreign "manifold_material" (ptr void @-> returning (ptr Material.t))

  let material_set_roughness =
    foreign "manifold_material_set_roughness" (ptr Material.t @-> float @-> returning void)

  let material_set_metalness =
    foreign "manifold_material_set_metalness" (ptr Material.t @-> float @-> returning void)

  let material_set_color =
    foreign "manifold_material_set_color" (ptr Material.t @-> Vec4.t @-> returning void)

  let material_set_vert_color =
    foreign
      "manifold_material_set_vert_color"
      (ptr Material.t @-> ptr Vec4.t @-> size_t @-> returning void)

  let export_options =
    foreign "manifold_export_options" (ptr void @-> returning (ptr ExportOptions.t))

  let export_options_set_faceted =
    foreign
      "manifold_export_options_set_faceted"
      (ptr ExportOptions.t @-> int @-> returning void)

  let export_options_set_material =
    foreign
      "manifold_export_options_set_material"
      (ptr ExportOptions.t @-> ptr Material.t @-> returning void)

  let export_meshgl =
    foreign
      "manifold_export_meshgl"
      (ptr char @-> ptr MeshGL.t @-> ptr ExportOptions.t @-> returning void)

  (* Sizes for allocation *)

  let manifold_size = foreign "manifold_manifold_size" (void @-> returning size_t)

  let simple_polygon_size =
    foreign "manifold_simple_polygon_size" (void @-> returning size_t)

  let polygons_size = foreign "manifold_polygons_size" (void @-> returning size_t)
  let pair_size = foreign "manifold_manifold_pair_size" (void @-> returning size_t)
  let meshgl_size = foreign "manifold_meshgl_size" (void @-> returning size_t)
  let box_size = foreign "manifold_box_size" (void @-> returning size_t)
  let curvature_size = foreign "manifold_curvature_size" (void @-> returning size_t)
  let components_size = foreign "manifold_components_size" (void @-> returning size_t)
  let material_size = foreign "manifold_material_size" (void @-> returning size_t)

  let export_options_size =
    foreign "manifold_export_options_size" (void @-> returning size_t)

  (* Destruction *)

  let destruct_manifold =
    foreign "manifold_destruct_manifold" (ptr Manifold.t @-> returning void)

  let destruct_simple_polygon =
    foreign "manifold_destruct_simple_polygon" (ptr SimplePolygon.t @-> returning void)

  let destruct_polygons =
    foreign "manifold_destruct_polygons" (ptr Polygons.t @-> returning void)

  let destruct_meshgl =
    foreign "manifold_destruct_meshgl" (ptr MeshGL.t @-> returning void)

  let destruct_box = foreign "manifold_destruct_box" (ptr Box.t @-> returning void)

  let destruct_curvature =
    foreign "manifold_destruct_curvature" (ptr Curvature.t @-> returning void)

  let destruct_components =
    foreign "manifold_destruct_components" (ptr Components.t @-> returning void)

  let destruct_material =
    foreign "manifold_destruct_material" (ptr Material.t @-> returning void)

  let destruct_export_options =
    foreign "manifold_destruct_export_options" (ptr ExportOptions.t @-> returning void)

  (* Deletion / Free *)

  let delete_manifold =
    foreign "manifold_delete_manifold" (ptr Manifold.t @-> returning void)

  let delete_simple_polygon =
    foreign "manifold_delete_simple_polygon" (ptr SimplePolygon.t @-> returning void)

  let delete_polygons =
    foreign "manifold_delete_polygons" (ptr Polygons.t @-> returning void)

  let delete_meshgl = foreign "manifold_delete_meshgl" (ptr MeshGL.t @-> returning void)
  let delete_box = foreign "manifold_delete_box" (ptr Box.t @-> returning void)

  let delete_curvature =
    foreign "manifold_delete_curvature" (ptr Curvature.t @-> returning void)

  let delete_components =
    foreign "manifold_delete_components" (ptr Components.t @-> returning void)

  let delete_material =
    foreign "manifold_delete_material" (ptr Material.t @-> returning void)

  let delete_export_options =
    foreign "manifold_delete_export_options" (ptr ExportOptions.t @-> returning void)
end
