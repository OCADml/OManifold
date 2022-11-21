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
    let _ = C.Funcs.manifold_export_options buf in
    Option.iter
      (fun b -> C.Funcs.manifold_export_options_set_faceted t (Bool.to_int b))
      faceted;
    Option.iter (fun m -> C.Funcs.manifold_export_options_set_material t m) material;
    t
end

let export_mesh ?(opts = Opts.make ()) path mesh =
  C.Funcs.manifold_export_mesh (Conv.string_to_ptr Ctypes_static.char path) mesh opts
