type mrect = C.Types.Rect.t Ctypes_static.ptr
type cross_section = C.Types.CrossSection.t Ctypes_static.ptr

type op_type =
  [ `Add
  | `Subtract
  | `Intersect
  ]

module Quality = Quality
module Manifold = Manifold
module Sdf2 = Sdf2
module Sdf3 = Sdf3
module MBox = MBox
module Export = Export
module MMeshGL = MMeshGL
module Curvature = Curvature
module CrossSection = CrossSection

module MRect = struct
  include MRect

  let to_cross_section t =
    let buf, cs = CrossSection.alloc () in
    let _ = C.Funcs.rect_as_cross_section buf t in
    cs
end
