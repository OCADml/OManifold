open Conv

module Material = struct
  type t = C.Types.Material.t Ctypes_static.ptr

  let size = C.Funcs.material_size () |> Conv.size_to_int
  let destruct t = C.Funcs.destruct_material t

  let alloc () =
    let finalise = Mem.finaliser C.Types.Material.t destruct in
    let buf = Mem.allocate_buf ~finalise size in
    buf, Conv.voidp_coerce Ctypes_static.(ptr C.Types.Material.t) buf

  let make ?roughness ?metalness ?color ?vert_color () =
    let buf, t = alloc () in
    let _ = C.Funcs.material buf in
    Option.iter (fun r -> C.Funcs.material_set_roughness t r) roughness;
    Option.iter (fun m -> C.Funcs.material_set_metalness t m) metalness;
    Option.iter (fun c -> C.Funcs.material_set_color t (vec4_of_v4 c)) color;
    Option.iter
      (fun vc ->
        let len = List.length vc in
        let sz = Conv.size_of_int len in
        let a = Ctypes.CArray.make C.Types.Vec4.t len in
        List.iteri (fun i c -> Ctypes.CArray.set a i (vec4_of_v4 c)) vc;
        C.Funcs.material_set_vert_color t (Ctypes.CArray.start a) sz )
      vert_color;
    t
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

let mmeshgl ?(opts = Opts.make ()) path mesh =
  C.Funcs.export_meshgl (Conv.string_to_ptr Ctypes_static.char path) mesh opts

let manifold ?opts path man = mmeshgl ?opts path (Manifold.to_mmeshgl man)
