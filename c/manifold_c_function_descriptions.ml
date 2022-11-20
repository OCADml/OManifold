module Descriptions (F : Ctypes.FOREIGN) = struct
  open! Ctypes
  open! F
  open! Manifold_c_types

  (* Polygons *)

  let manifold_simple_polygon =
    foreign
      "manifold_simple_polygon"
      (ptr void @-> ptr Vec2.t @-> size_t @-> returning (ptr SimplePolygon.t))

  let manifold_polygons =
    foreign
      "manifold_polygons"
      (ptr void @-> ptr SimplePolygon.t @-> size_t @-> returning (ptr Polygons.t))

  (* Mesh Construction *)

  let manifold_mesh =
    foreign
      "manifold_mesh"
      ( ptr void
      @-> ptr Vec3.t
      @-> size_t
      @-> ptr IVec3.t
      @-> size_t
      @-> returning (ptr Mesh.t) )

  let manifold_mesh_w_normals =
    foreign
      "manifold_mesh_w_normals"
      ( ptr void
      @-> ptr Vec3.t
      @-> size_t
      @-> ptr IVec3.t
      @-> size_t
      @-> ptr Vec3.t
      @-> returning (ptr Mesh.t) )

  let manifold_mesh_w_tangents =
    foreign
      "manifold_mesh_w_tangents"
      ( ptr void
      @-> ptr Vec3.t
      @-> size_t
      @-> ptr IVec3.t
      @-> size_t
      @-> ptr Vec4.t
      @-> returning (ptr Mesh.t) )

  let manifold_mesh_w_normals_tangents =
    foreign
      "manifold_mesh_w_normals_tangents"
      ( ptr void
      @-> ptr Vec3.t
      @-> size_t
      @-> ptr IVec3.t
      @-> size_t
      @-> ptr Vec3.t
      @-> ptr Vec4.t
      @-> returning (ptr Mesh.t) )

  (* ManifoldMesh *manifold_get_mesh(void *mem, ManifoldManifold *m); *)
  (* ManifoldMeshGL *manifold_get_meshgl(void *mem, ManifoldManifold *m); *)

  (* Mesh Info  *)

  (* ManifoldManifold *manifold_as_original(void *mem, ManifoldManifold *m); *)
  (* int manifold_original_id(ManifoldManifold *m); *)
  (* ManifoldMeshRelation *manifold_get_mesh_relation(void *mem, *)
  (*                                                  ManifoldManifold *m); *)
  (* size_t manifold_mesh_relation_barycentric_length(ManifoldMeshRelation *m); *)
  (* ManifoldVec3 *manifold_mesh_relation_barycentric(void *mem, *)
  (*                                                  ManifoldMeshRelation *m); *)

  (* size_t manifold_mesh_vert_length(ManifoldMesh *m); *)
  (* size_t manifold_mesh_tri_length(ManifoldMesh *m); *)
  (* size_t manifold_mesh_normal_length(ManifoldMesh *m); *)
  (* size_t manifold_mesh_tangent_length(ManifoldMesh *m); *)
  (* ManifoldVec3 *manifold_mesh_vert_pos(void *mem, ManifoldMesh *m); *)
  (* ManifoldIVec3 *manifold_mesh_tri_verts(void *mem, ManifoldMesh *m); *)
  (* ManifoldVec3 *manifold_mesh_vert_normal(void *mem, ManifoldMesh *m); *)
  (* ManifoldVec4 *manifold_mesh_halfedge_tangent(void *mem, ManifoldMesh *m); *)

  (* size_t manifold_meshgl_vert_length(ManifoldMeshGL *m); *)
  (* size_t manifold_meshgl_tri_length(ManifoldMeshGL *m); *)
  (* size_t manifold_meshgl_normal_length(ManifoldMeshGL *m); *)
  (* size_t manifold_meshgl_tangent_length(ManifoldMeshGL *m); *)
  (* float *manifold_meshgl_vert_pos(void *mem, ManifoldMeshGL *m); *)
  (* uint32_t *manifold_meshgl_tri_verts(void *mem, ManifoldMeshGL *m); *)
  (* float *manifold_meshgl_vert_normal(void *mem, ManifoldMeshGL *m); *)
  (* float *manifold_meshgl_halfedge_tangent(void *mem, ManifoldMeshGL *m); *)

  (* Manifold Shapes / Constructors *)

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

  let manifold_of_mesh =
    foreign "manifold_of_mesh" (ptr void @-> ptr Mesh.t @-> returning (ptr Manifold.t))

  let manifold_of_mesh_props =
    foreign
      "manifold_of_mesh_props"
      ( ptr void
      @-> ptr Mesh.t
      @-> ptr IVec3.t
      @-> ptr float
      @-> ptr float
      @-> size_t
      @-> returning (ptr Manifold.t) )

  let manifold_smooth =
    foreign
      "manifold_smooth"
      ( ptr void
      @-> ptr Mesh.t
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
      (ptr void @-> ptr Manifold.t @-> size_t @-> returning (ptr Manifold.t))

  let manifold_decompose_length =
    foreign "manifold_decompose_length" (ptr Manifold.t @-> returning size_t)

  let manifold_decompose =
    foreign
      "manifold_decompose"
      (ptr void @-> ptr Manifold.t @-> size_t @-> returning (ptr Manifold.t))

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
  (* void manifold_set_min_circular_angle(ManifoldManifold *m, float degrees); *)
  (* void manifold_set_min_circular_edge_length(ManifoldManifold *m, float length); *)
  (* void manifold_set_circular_segments(ManifoldManifold *m, int number); *)

  (* Manifold Info *)
  (* int manifold_is_empty(ManifoldManifold *m); *)
  (* ManifoldError manifold_status(ManifoldManifold *m); *)
  (* int manifold_num_vert(ManifoldManifold *m); *)
  (* int manifold_num_edge(ManifoldManifold *m); *)
  (* int manifold_num_tri(ManifoldManifold *m); *)
  (* ManifoldBox *manifold_bounding_box(void *mem, ManifoldManifold *m); *)
  (* float manifold_precision(ManifoldManifold *m); *)
  (* int manifold_genus(ManifoldManifold *m); *)
  (* ManifoldCurvature *manifold_get_curvature(void *mem, ManifoldManifold *m); *)
  (* ManifoldCurvatureBounds manifold_curvature_bounds(ManifoldCurvature *curv); *)
  (* size_t manifold_curvature_vert_length(ManifoldCurvature *curv); *)
  (* float *manifold_curvature_vert_mean(void *mem, ManifoldCurvature *curv); *)
  (* float *manifold_curvature_vert_gaussian(void *mem, ManifoldCurvature *curv); *)
  (* int manifold_get_circular_segments(ManifoldManifold *m, float radius); *)

  (* Bounding Box *)

  let manifold_box =
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

  (* ManifoldBox *manifold_box(void *mem, float x1, float y1, float z1, float x2, *)
  (*                           float y2, float z2); *)
  (* ManifoldVec3 manifold_box_min(ManifoldBox *b); *)
  (* ManifoldVec3 manifold_box_max(ManifoldBox *b); *)
  (* ManifoldVec3 manifold_box_dimensions(ManifoldBox *b); *)
  (* ManifoldVec3 manifold_box_center(ManifoldBox *b); *)
  (* float manifold_box_scale(ManifoldBox *b); *)
  (* int manifold_box_contains_pt(ManifoldBox *b, float x, float y, float z); *)
  (* int manifold_box_contains_box(ManifoldBox *a, ManifoldBox *b); *)
  (* void manifold_box_include_pt(ManifoldBox *b, float x, float y, float z); *)
  (* ManifoldBox *manifold_box_union(void *mem, ManifoldBox *a, ManifoldBox *b); *)
  (* ManifoldBox *manifold_box_transform(void *mem, ManifoldBox *b, float x1, *)
  (*                                     float y1, float z1, float x2, float y2, *)
  (*                                     float z2, float x3, float y3, float z3, *)
  (*                                     float x4, float y4, float z4); *)
  (* ManifoldBox *manifold_box_translate(void *mem, ManifoldBox *b, float x, float y, *)
  (*                                     float z); *)
  (* ManifoldBox *manifold_box_mul(void *mem, ManifoldBox *b, float x, float y, *)
  (*                               float z); *)
  (* int manifold_box_does_overlap_pt(ManifoldBox *b, float x, float y, float z); *)
  (* int manifold_box_does_overlap_box(ManifoldBox *a, ManifoldBox *b); *)
  (* int manifold_box_is_finite(ManifoldBox *b); *)

  (* SDF *)

  let sdf_t = Ctypes.(float @-> float @-> float @-> returning float)

  let manifold_level_set =
    foreign
      "manifold_level_set"
      ( ptr void
      @-> static_funptr sdf_t
      @-> ptr Box.t
      @-> float
      @-> float
      @-> returning (ptr Mesh.t) )

  (* Export *)
  (* ManifoldMaterial *manifold_material(void *mem); *)
  (* void manifold_material_set_roughness(ManifoldMaterial *mat, float roughness); *)
  (* void manifold_material_set_metalness(ManifoldMaterial *mat, float metalness); *)
  (* void manifold_material_set_color(ManifoldMaterial *mat, ManifoldVec4 color); *)
  (* void manifold_material_set_vert_color(ManifoldMaterial *mat, *)
  (*                                       ManifoldVec4 *vert_color, size_t n_vert); *)

  (* ManifoldExportOptions *manifold_export_options(void *mem); *)

  (* void manifold_export_options_set_faceted(ManifoldExportOptions *options, *)
  (*                                          int faceted); *)
  (* void manifold_export_options_set_material(ManifoldExportOptions *options, *)
  (*                                           ManifoldMaterial *mat); *)
  (* void manifold_export_mesh(char *filename, ManifoldMesh *mesh, *)
  (*                           ManifoldExportOptions *options); *)

  (* Sizes for allocation *)

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

  (* Deletion / Free *)

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
