module Types (F : Cstubs.Types.TYPE) = struct
  open Ctypes
  open F

  module Status = struct
    let no_error = constant "MANIFOLD_NO_ERROR" int64_t
    let non_finite_vertex = constant "MANIFOLD_NON_FINITE_VERTEX" int64_t
    let not_manifold = constant "MANIFOLD_NOT_MANIFOLD" int64_t

    let vertex_index_out_of_bounds =
      constant "MANIFOLD_VERTEX_INDEX_OUT_OF_BOUNDS" int64_t

    let properties_wrong_length = constant "MANIFOLD_PROPERTIES_WRONG_LENGTH" int64_t

    let missing_position_properties =
      constant "MANIFOLD_MISSING_POSITION_PROPERTIES" int64_t

    let merge_vectors_different_lengths =
      constant "MANIFOLD_MERGE_VECTORS_DIFFERENT_LENGTHS" int64_t

    let merge_index_out_of_bounds = constant "MANIFOLD_MERGE_INDEX_OUT_OF_BOUNDS" int64_t
    let transform_wrong_length = constant "MANIFOLD_TRANSFORM_WRONG_LENGTH" int64_t
    let run_index_wrong_length = constant "MANIFOLD_RUN_INDEX_WRONG_LENGTH" int64_t
    let face_id_wrong_length = constant "MANIFOLD_FACE_ID_WRONG_LENGTH" int64_t

    type t =
      | NoError
      | NonFiniteVertex
      | NotManifold
      | VertexIndexOutOfBounds
      | PropertiesWrongLength
      | MissingPositionProperties
      | MergeVectorsDifferentLengths
      | MergeIndexOutOfBounds
      | TransformWrongLength
      | RunIndexWrongLength
      | FaceIDWrongLength

    let t =
      enum
        "ManifoldError"
        [ NoError, no_error
        ; NonFiniteVertex, non_finite_vertex
        ; NotManifold, not_manifold
        ; VertexIndexOutOfBounds, vertex_index_out_of_bounds
        ; PropertiesWrongLength, properties_wrong_length
        ; MissingPositionProperties, missing_position_properties
        ; MergeVectorsDifferentLengths, merge_vectors_different_lengths
        ; MergeIndexOutOfBounds, merge_index_out_of_bounds
        ; TransformWrongLength, transform_wrong_length
        ; RunIndexWrongLength, run_index_wrong_length
        ; FaceIDWrongLength, face_id_wrong_length
        ]
        ~unexpected:(fun _ -> assert false)
  end

  module Manifold = struct
    type t = [ `Manifold ] structure

    let t : t typ = structure "ManifoldManifold"
  end

  module FillRule = struct
    let even_odd = constant "MANIFOLD_FILL_RULE_EVEN_ODD" int64_t
    let non_zero = constant "MANIFOLD_FILL_RULE_NON_ZERO" int64_t
    let positive = constant "MANIFOLD_FILL_RULE_POSITIVE" int64_t
    let negative = constant "MANIFOLD_FILL_RULE_NEGATIVE" int64_t

    type t =
      | EvenOdd
      | NonZero
      | Positive
      | Negative

    let t =
      enum
        "ManifoldFillRule"
        [ EvenOdd, even_odd; NonZero, non_zero; Positive, positive; Negative, negative ]
        ~unexpected:(fun _ -> assert false)
  end

  module JoinType = struct
    let square = constant "MANIFOLD_JOIN_TYPE_SQUARE" int64_t
    let round = constant "MANIFOLD_JOIN_TYPE_ROUND" int64_t
    let miter = constant "MANIFOLD_JOIN_TYPE_MITER" int64_t

    type t =
      | Square
      | Round
      | Miter

    let t =
      enum
        "ManifoldJoinType"
        [ Square, square; Round, round; Miter, miter ]
        ~unexpected:(fun _ -> assert false)
  end

  module CrossSection = struct
    type t = [ `CrossSection ] structure

    let t : t typ = structure "ManifoldCrossSection"
  end

  module SimplePolygon = struct
    type t = [ `SimplePolygon ] structure

    let t : t typ = structure "ManifoldSimplePolygon"
  end

  module Polygons = struct
    type t = [ `Polygons ] structure

    let t : t typ = structure "ManifoldPolygons"
  end

  module MeshGL = struct
    type t = [ `MeshGL ] structure

    let t : t typ = structure "ManifoldMeshGL"
  end

  module Curvature = struct
    type t = [ `Curvature ] structure

    let t : t typ = structure "ManifoldCurvature"
  end

  module Components = struct
    type t = [ `Components ] structure

    let t : t typ = structure "ManifoldComponents"
  end

  module Properties = struct
    type t = [ `Properties ] structure

    let t : t typ = structure "ManifoldProperties"
    let surface_area = field t "surface_area" float
    let volume = field t "volume" float
    let () = seal t
  end

  module Box = struct
    type t = [ `Box ] structure

    let t : t typ = structure "ManifoldBox"
  end

  module Rect = struct
    type t = [ `Rect ] structure

    let t : t typ = structure "ManifoldRect"
  end

  module Material = struct
    type t = [ `Material ] structure

    let t : t typ = structure "ManifoldMaterial"
  end

  module ExportOptions = struct
    type t = [ `ExportOptions ] structure

    let t : t typ = structure "ManifoldExportOptions"
  end

  module ManifoldPair = struct
    type t = [ `ManifoldPair ] structure

    let t : t typ = structure "ManifoldManifoldPair"
    let first = field t "first" (ptr Manifold.t)
    let second = field t "second" (ptr Manifold.t)
    let () = seal t
  end

  module Vec2 = struct
    type t = [ `Vec2 ] structure

    let t : t typ = structure "ManifoldVec2"
    let x = field t "x" float
    let y = field t "y" float
    let () = seal t
  end

  module Vec3 = struct
    type t = [ `Vec3 ] structure

    let t : t typ = structure "ManifoldVec3"
    let x = field t "x" float
    let y = field t "y" float
    let z = field t "z" float
    let () = seal t
  end

  module IVec3 = struct
    type t = [ `IVec3 ] structure

    let t : t typ = structure "ManifoldIVec3"
    let x = field t "x" int
    let y = field t "y" int
    let z = field t "z" int
    let () = seal t
  end

  module Vec4 = struct
    type t = [ `Vec4 ] structure

    let t : t typ = structure "ManifoldVec4"
    let x = field t "x" float
    let y = field t "y" float
    let z = field t "z" float
    let w = field t "w" float
    let () = seal t
  end

  module CurvatureBounds = struct
    type t = [ `CurvatureBounds ] structure

    let t : t typ = structure "ManifoldCurvatureBounds"
    let max_mean_curvature = field t "max_mean_curvature" float
    let min_mean_curvature = field t "min_mean_curvature" float
    let max_gaussian_curvature = field t "max_gaussian_curvature" float
    let min_gaussian_curvature = field t "min_gaussian_curvature" float
    let () = seal t
  end
end
