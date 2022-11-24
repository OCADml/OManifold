open Conv

module Bounds = struct
  module CB = C.Types.CurvatureBounds

  type t =
    { min_mean : float
    ; max_mean : float
    ; min_gaussian : float
    ; max_gaussian : float
    }

  let of_struct s =
    let max_mean = Ctypes.getf s CB.max_mean_curvature
    and min_mean = Ctypes.getf s CB.min_mean_curvature
    and max_gaussian = Ctypes.getf s CB.max_gaussian_curvature
    and min_gaussian = Ctypes.getf s CB.min_gaussian_curvature in
    { min_mean; max_mean; min_gaussian; max_gaussian }

  let to_struct t =
    let s = Ctypes.make CB.t in
    Ctypes.setf s CB.max_mean_curvature t.max_mean;
    Ctypes.setf s CB.min_mean_curvature t.min_mean;
    Ctypes.setf s CB.max_gaussian_curvature t.max_gaussian;
    Ctypes.setf s CB.min_gaussian_curvature t.min_gaussian;
    s
end

type ptr = C.Types.Curvature.t Ctypes_static.ptr

type t =
  { bounds : Bounds.t
  ; vert_mean : float list
  ; vert_gaussian : float list
  }

let size = C.Funcs.curvature_size () |> size_to_int
let destruct t = C.Funcs.destruct_curvature t

let alloc () =
  let finalise = Mem.finaliser C.Types.Curvature.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes.coerce Ctypes_static.(ptr void) Ctypes_static.(ptr C.Types.Curvature.t) buf

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

let of_ptr p =
  { bounds = bounds p; vert_mean = vert_mean p; vert_gaussian = vert_gaussian p }
