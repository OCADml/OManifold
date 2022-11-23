module Material = struct
  (* ManifoldMaterial *manifold_material(void *mem); *)
  (* void manifold_material_set_roughness(ManifoldMaterial *mat, float roughness); *)
  (* void manifold_material_set_metalness(ManifoldMaterial *mat, float metalness); *)
  (* void manifold_material_set_color(ManifoldMaterial *mat, ManifoldVec4 color); *)
  (* void manifold_material_set_vert_color(ManifoldMaterial *mat, *)
  (*                                       ManifoldVec4 *vert_color, size_t n_vert); *)
end

module Opts = struct
  type t = C.Types.ExportOptions.t Ctypes_static.ptr

  let size = C.Funcs.export_options_size () |> Conv.size_to_int
  let destruct t = C.Funcs.destruct_export_options t

  let alloc () =
    let finalise = Mem.finaliser C.Types.ExportOptions.t destruct in
    let buf = Mem.allocate_buf ~finalise size in
    buf, Conv.voidp_coerce Ctypes_static.(ptr C.Types.ExportOptions.t) buf

  let make ?faceted ?material () =
    let buf, t = alloc () in
    let _ = C.Funcs.export_options buf in
    Option.iter (fun b -> C.Funcs.export_options_set_faceted t (Bool.to_int b)) faceted;
    Option.iter (fun m -> C.Funcs.export_options_set_material t m) material;
    t
end

let export_mesh ?(opts = Opts.make ()) path mesh =
  C.Funcs.export_mesh (Conv.string_to_ptr Ctypes_static.char path) mesh opts
