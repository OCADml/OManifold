open OCADml
open Conv

let size = C.Funcs.manifold_size () |> size_to_int
let destruct m = C.Funcs.destruct_manifold m

type t = C.Types.Manifold.t Ctypes_static.ptr

let pair_to_tup p = C.Types.ManifoldPair.(Ctypes.getf p first, Ctypes.getf p second)

let alloc () =
  let finalise = Mem.finaliser C.Types.Manifold.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.Manifold.t) buf

let status t = C.Funcs.manifold_status t

let tetrahedron () =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_tetrahedron buf in
  t

let sphere ?(fn = 32) rad =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_sphere buf rad fn in
  t

let union a b =
  let buf, t = alloc () in
  let _ = C.Funcs.manifold_union buf a b in
  t

let warp f t =
  let buf, warped = alloc () in
  let f x y z = Conv.vec3_of_v3 (f (v3 x y z)) in
  let f =
    Ctypes.(coerce (Foreign.funptr C.Funcs.warp_t) (static_funptr C.Funcs.warp_t) f)
  in
  let _ = C.Funcs.manifold_warp buf t f in
  warped
