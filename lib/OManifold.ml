type manifold = C.Types.Manifold.t Ctypes_static.ptr
type mmeshgl = C.Types.MeshGL.t Ctypes_static.ptr
type cross_section = C.Types.CrossSection.t Ctypes_static.ptr

type op_type =
  [ `Add
  | `Subtract
  | `Intersect
  ]

module Quality = Quality
module Manifold = Manifold
module Cross = CrossSection
module Sdf2 = Sdf2
module Sdf3 = Sdf3
module MMeshGL = MMeshGL
module Export = Export
