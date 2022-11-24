open OCADml

module Curvature : sig
  module Bounds : sig
    type t =
      { min_mean : float
      ; max_mean : float
      ; min_gaussian : float
      ; max_gaussian : float
      }
  end

  type t =
    { bounds : Bounds.t
    ; vert_mean : float list
    ; vert_gaussian : float list
    }
end

module MeshRelation : sig
  module BaryRef : sig
    type t =
      { mesh_id : int
      ; original_id : int
      ; tri : int
      ; vert_bary : int * int * int
      }
  end

  type t =
    { barycentric : v3 list
    ; tri_bary : BaryRef.t list
    }
end

module PolyVert : sig
  (* TODO: I think this should go in the Polygon module (TBD) *)

  type t =
    { pos : v2
    ; idx : int
    }
end

module MMesh : sig
  type t

  (** {1 Constructor} *)

  val make
    :  ?normals:v3 list
    -> ?tangents:v4 list
    -> v3 list
    -> (int * int * int) list
    -> t

  (** {1 Data Extraction} *)

  val points : t -> v3 list
  val triangles : t -> (int * int * int) list
  val faces : t -> int list list
  val normals : t -> v3 list
  val halfedge_tangents : t -> v4 list

  (** {1 OCADml conversion} *)

  val of_mesh : ?normals:v3 list -> ?tangents:v4 list -> Mesh.t -> t
  val to_mesh : t -> Mesh.t
end

module MMeshGL : sig
  type t

  (** {1 Data Extraction} *)

  val points : t -> float list
  val triangles : t -> int list
  val normals : t -> float list
  val halfedge_tangents : t -> float list
end

module Box : sig
  type t

  (** {1 Constructor} *)

  val make : v3 -> v3 -> t

  (** {1 Data / Queries} *)

  val min : t -> v3
  val max : t -> v3
  val center : t -> v3
  val dimensions : t -> v3
  val abs_max_coord : t -> float
  val contains_pt : t -> v3 -> bool
  val contains_box : t -> t -> bool
  val overlaps_pt : t -> v3 -> bool
  val overlaps_box : t -> t -> bool
  val is_finite : t -> bool

  (** {1 Booleans} *)

  val union : t -> t -> t
  val include_pt : t -> v3 -> unit

  (** {1 Transformations} *)

  val translate : v3 -> t -> t
  val scale : v3 -> t -> t
  val transform : Affine3.t -> t -> t

  (** {1 OCADml conversion}*)

  val of_bbox : V3.bbox -> t
  val to_bbox : t -> V3.bbox
end

module Manifold : sig
  module Status : sig
    type t = C.Types.Status.t =
      | NoError
      | NonFiniteVertex
      | NotManifold
      | VertexIndexOutOfBounds
      | PropertiesWrongLength
      | TriPropertiesWrongLength
      | TriPropertiesOutOfBounds

    val to_string : t -> string
  end

  type t

  (** {1 Basic Constructors} *)

  val empty : unit -> t
  val copy : t -> t
  val as_original : t -> t
  val compose : t list -> t
  val decompose : t -> t list

  (** {1 Data Extraction} *)

  val status : t -> Status.t
  val original_id : t -> int
  val num_vert : t -> int
  val num_edge : t -> int
  val num_tri : t -> int
  val bounding_box : t -> Box.t
  val precision : t -> float
  val genus : t -> int
  val circular_segments : t -> float -> int
  val curvature : t -> Curvature.t
  val mesh_relation : t -> MeshRelation.t
  val points : t -> v3 list

  (** {1 Shapes} *)

  val tetrahedron : unit -> t
  val sphere : ?fn:int -> float -> t
  val cube : ?center:bool -> v3 -> t
  val cylinder : ?center:bool -> ?fn:int -> height:float -> float -> t
  val cone : ?center:bool -> ?fn:int -> height:float -> float -> float -> t

  (** {1 Mesh Conversions} *)

  val of_mmesh : MMesh.t -> t
  val of_mesh : ?normals:v3 list -> ?tangents:v4 list -> Mesh.t -> t
  val smooth : ?smoothness:(int * float) list -> MMesh.t -> t
  val to_mmesh : t -> MMesh.t
  val to_mmeshgl : t -> MMeshGL.t
  val to_mesh : t -> Mesh.t

  (** {1 2D to 3D} *)

  val extrude
    :  ?slices:int
    -> ?twist:float
    -> ?scale:v2
    -> height:float
    -> Manifold_c_types.Polygons.t Ctypes_static.ptr
    -> t

  val revolve : ?fn:int -> Manifold_c_types.Polygons.t Ctypes_static.ptr -> t

  (** {1 Booleans} *)

  val add : t -> t -> t
  val sub : t -> t -> t
  val intersect : t -> t -> t
  val union : t list -> t
  val difference : t -> t list -> t
  val intersection : t -> t list -> t
  val split : t -> t -> t * t
  val split_by_plane : Plane.t -> t -> t * t
  val trim_by_plane : Plane.t -> t -> t

  (** {1 Transformations} *)

  val warp : (v3 -> v3) -> t -> t
  val translate : v3 -> t -> t
  val xtrans : float -> t -> t
  val ytrans : float -> t -> t
  val ztrans : float -> t -> t
  val rotate : ?about:v3 -> v3 -> t -> t
  val xrot : float -> t -> t
  val yrot : float -> t -> t
  val zrot : float -> t -> t
  val affine : Affine3.t -> t -> t
  val quaternion : ?about:v3 -> Quaternion.t -> t -> t
  val axis_rotate : ?about:v3 -> v3 -> float -> t -> t
  val scale : v3 -> t -> t
  val xscale : float -> t -> t
  val yscale : float -> t -> t
  val zscale : float -> t -> t
  val refine : int -> t -> t
  val hull : t list -> t

  (** {1 Mutations} *)

  val set_circular_segments : t -> int -> unit
  val set_min_circular_angle : t -> float -> unit
  val set_min_circular_edge_length : t -> float -> unit
end

module Sdf : sig
  type t = v3 -> float

  (** {1 Shapes} *)

  val sphere : float -> t
  val cube : ?round:float -> v3 -> t
  val torus : v2 -> t
  val cylinder : ?round:float -> height:float -> float -> t

  (** {1 Transformations} *)

  val translate : v3 -> t -> t
  val rotate : ?about:v3 -> v3 -> t -> t
  val quaternion : ?about:v3 -> Quaternion.t -> t -> t
  val axis_rotate : ?about:v3 -> v3 -> float -> t -> t
  val scale : float -> t -> t
  val round : float -> t -> t
  val elongate : v3 -> t -> t

  (** {1 Booleans} *)

  val union : ?smooth:float -> t -> t -> t
  val difference : ?smooth:float -> t -> t -> t
  val intersection : ?smooth:float -> t -> t -> t

  (** {1 Mesh Generation} *)

  val to_mmesh : ?level:float -> ?edge_length:float -> box:Box.t -> t -> MMesh.t
end

module Export : sig
  module Material : sig
    type t

    val make
      :  ?roughness:float
      -> ?metalness:float
      -> ?color:v4
      -> ?vert_color:v4 list
      -> unit
      -> t
  end

  module Opts : sig
    type t

    val make : ?faceted:bool -> ?material:Material.t -> unit -> t
  end

  val mmesh : ?opts:Opts.t -> string -> MMesh.t -> unit
  val manifold : ?opts:Opts.t -> string -> Manifold.t -> unit
end
