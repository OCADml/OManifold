open Conv

module Bounds = struct
  module CB = C.Types.CurvatureBounds

  type t =
    { max_mean_curvature : float
    ; min_mean_curvature : float
    ; max_gaussian_curvature : float
    ; min_gaussian_curvature : float
    }

  let of_struct s =
    let max_mean_curvature = Ctypes.getf s CB.max_mean_curvature
    and min_mean_curvature = Ctypes.getf s CB.min_mean_curvature
    and max_gaussian_curvature = Ctypes.getf s CB.max_gaussian_curvature
    and min_gaussian_curvature = Ctypes.getf s CB.min_gaussian_curvature in
    { max_mean_curvature
    ; min_mean_curvature
    ; max_gaussian_curvature
    ; min_gaussian_curvature
    }

  let to_struct t =
    let s = Ctypes.make CB.t in
    Ctypes.setf s CB.max_mean_curvature t.max_mean_curvature;
    Ctypes.setf s CB.min_mean_curvature t.min_mean_curvature;
    Ctypes.setf s CB.max_gaussian_curvature t.max_gaussian_curvature;
    Ctypes.setf s CB.min_gaussian_curvature t.min_gaussian_curvature;
    s
end

type t = C.Types.Curvature.t Ctypes_static.ptr

let size = C.Funcs.curvature_size () |> size_to_int
let destruct t = C.Funcs.destruct_curvature t

let alloc () =
  let finalise = Mem.finaliser C.Types.Curvature.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.Curvature.t) buf

(* TODO: should bundle into one type and just do all of the conversion to
    return an ocaml struct from manifold get curvature *)
let bounds t = Bounds.of_struct (C.Funcs.curvature_bounds t)
let vert_length t = C.Funcs.curvature_vert_length t

let vert_mean t =
  let len = size_to_int @@ vert_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in
  let means = C.Funcs.curvature_vert_mean buf t in
  List.init len Ctypes.(fun i -> !@(means +@ i))

let vert_gaussian t =
  let len = size_to_int @@ vert_length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof float)) in
  let gaussians = C.Funcs.curvature_vert_gaussian buf t in
  List.init len Ctypes.(fun i -> !@(gaussians +@ i))
