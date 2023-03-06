open! OCADml
open Conv

type t = C.Types.CrossSection.t Ctypes_static.ptr

let size = C.Funcs.cross_section_size () |> size_to_int
let destruct t = C.Funcs.destruct_cross_section t

let alloc () =
  let finalise = Mem.finaliser C.Types.CrossSection.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  ( buf
  , Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.CrossSection.t) buf
  )

let of_simple_polygon ~fill_rule sp =
  let buf, t = alloc () in
  let _ = C.Funcs.cross_section_of_simple_polygon buf sp fill_rule in
  t

let of_polygons ~fill_rule ps =
  let buf, t = alloc () in
  let _ = C.Funcs.cross_section_of_polygons buf ps fill_rule in
  t
