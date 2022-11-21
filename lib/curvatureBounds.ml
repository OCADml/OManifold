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
