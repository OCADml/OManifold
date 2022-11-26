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

module Polygons : sig
  type t

  val make : Path2.t list -> t
  val of_poly2 : Poly2.t -> t
  val of_poly2s : Poly2.t list -> t
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
  val normals : t -> v3 list
  val halfedge_tangents : t -> v4 list

  (** {1 OCADml conversion}

      To and from the OCADml [Mesh.t] type. As [Mesh.t] follows the OpenSCAD
      winding convention, which is opposite to Manifold, the triangles/faces are
      reversed. *)

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
    -> Polygons.t
    -> t

  val revolve : ?fn:int -> Polygons.t -> t

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

module Sdf2 : sig
  type t = OCADml.v2 -> float

  (** {1 Shapes} *)

  val circle : float -> t
  val square : ?round:float -> v2 -> t
  val rounded_box : ?tl:float -> ?tr:float -> ?bl:float -> ?br:float -> v2 -> v2 -> float
  val rhombus : ?round:float -> v2 -> t

  (** {1 Transformations} *)

  val translate : v2 -> t -> t
  val xtrans : float -> t -> t
  val ytrans : float -> t -> t
  val rotate : ?about:v2 -> float -> t -> t
  val zrot : ?about:v2 -> float -> t -> t
  val scale : float -> t -> t
  val round : float -> t -> t
  val onion : float -> t -> t

  (** [elongate h]

       Elongate the 2d sdf [t] by the xy distance vector [h]. Basically, the
       field is split and moved apart by [h] in each dimension and connected. *)
  val elongate : v2 -> t -> t

  (** {1 2d to 3d} *)

  (** [extrude ~height t]

       Extrude the 2d signed distance field [t] into a 3d field extending
       [height /. 2.] above and below the xy plane. *)
  val extrude : height:float -> t -> Sdf3.t

  (** [revolve ?offset t]

       Revolve the 2d signed distance field [t] around the
       y-axis. If provided [offset] translates [t] in x beforehand. *)
  val revolve : ?offset:float -> t -> Sdf3.t
end

module Sdf3 : sig
  type t = v3 -> float

  (** {1 Shapes} *)

  val sphere : float -> t
  val cube : ?round:float -> v3 -> t
  val torus : v2 -> t
  val cylinder : ?round:float -> height:float -> float -> t

  (** {1 Transformations} *)

  val translate : v3 -> t -> t
  val xtrans : float -> t -> t
  val ytrans : float -> t -> t
  val ztrans : float -> t -> t
  val rotate : ?about:v3 -> v3 -> t -> t
  val xrot : ?about:v3 -> float -> t -> t
  val yrot : ?about:v3 -> float -> t -> t
  val zrot : ?about:v3 -> float -> t -> t
  val quaternion : ?about:v3 -> Quaternion.t -> t -> t
  val axis_rotate : ?about:v3 -> v3 -> float -> t -> t
  val scale : float -> t -> t
  val round : float -> t -> t
  val onion : float -> t -> t
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
