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

  (** {1 Constructors}*)

  (** [make paths]

       Create a set of polygons from a set of 2d paths. These can vary from
       arbitrarily nested to completely separate, describing one or more complex
       polygons with holes. *)
  val make : Path2.t list -> t

  (** {1 OCADml Conversion}

       These are nothing special, but are provided for convenience (prepending
       the outer path onto the list of holes before applying {!make}). *)

  (** [of_poly2 poly]

       Lift a 2d {!OCADml.Poly2.t} into a Manifold polygon set. *)
  val of_poly2 : Poly2.t -> t

  (** [of_poly2s polys]

       Lump together a set of 2d {!OCADml.Poly2.t}s into a Manifold polygon set. *)
  val of_poly2s : Poly2.t list -> t
end

module MMesh : sig
  (** The 3-dimensional mesh representation of the Manifold library. *)
  type t

  (** {1 Constructor} *)

  (** [make ?normals ?tangents points triangles]

       Create a Manifold mesh from a set of [points], and [triangles]
       represented as triples of indices into [points]. Vertex [normals] and
       half-edge [tangents] can optionally be provided. If so, they must be the
       same length as [points] and 3x the length of [triangles] respectively --
       [Invalid_argument] will be raised otherwise. *)
  val make
    :  ?normals:v3 list
    -> ?tangents:v4 list
    -> v3 list
    -> (int * int * int) list
    -> t

  (** {1 Data Extraction} *)

  (** [points t]

       Retrieve the points making up the mesh [t]. *)
  val points : t -> v3 list

  (** [triangles t]

       Retrieve the triangular faces of the mesh [t] as triples of indices into
       its {!points}. *)
  val triangles : t -> (int * int * int) list

  (** [normals t]

       Retrieve the vertex normals of the mesh [t] (same length as {!points}). *)
  val normals : t -> v3 list

  (** [halfedge_tangents t]

       Retrieve the halfedge tangents of the mesh [t] (three for each of the
       [t]'s {!triangles}). *)
  val halfedge_tangents : t -> v4 list

  (** {1 OCADml conversion}

      To and from the OCADml [Mesh.t] type. As [Mesh.t] follows the OpenSCAD
      winding convention (opposite to Manifold) the triangles/faces are
      reversed. *)

  (** [of_mesh m]

       Create a Manifold mesh from the [OCADml.Mesh.t] [m].  *)
  val of_mesh : Mesh.t -> t

  (** [to_mesh t]

       Create an [OCADml.Mesh.t] from the Manifold mesh [t].  *)
  val to_mesh : t -> Mesh.t
end

module MMeshGL : sig
  (** A graphics library friendly representation of Manifold's {!MMesh.t}.
       Obtained solely via {!Manifold.to_mmeshgl} *)
  type t

  (** {1 Data Extraction} *)

  val points : t -> float list
  val triangles : t -> int list
  val normals : t -> float list
  val halfedge_tangents : t -> float list
end

module Box : sig
  (** The 3-dimensional bounding box representation of the Manifold library. *)
  type t

  (** {1 Constructor} *)

  (** [make a b]

       Create a bounding box that contains the points [a] and [b]. *)
  val make : v3 -> v3 -> t

  (** {1 Data / Queries} *)

  (** [min t]

       The minimum bound of the box [t]. *)
  val min : t -> v3

  (** [max t]

       The maximum bound of the box [t]. *)
  val max : t -> v3

  (** [center t]

       The center point of the box [t]. *)
  val center : t -> v3

  (** [dimensions t]

       The 3-dimensional extent (xyz) of the box [t]. *)
  val dimensions : t -> v3

  (** [abs_max_coord t]

       The absolute largest dimensional coordinate of the box [t]. *)
  val abs_max_coord : t -> float

  (** [contains_pt t p]

       Determine whether the box [t] contains the point [p]. *)
  val contains_pt : t -> v3 -> bool

  (** [contains_box a b]

       Determine whether the box [a] contains the box [b]. *)
  val contains_box : t -> t -> bool

  (** [overlaps_pt t p]

       Determine whether the projection/shadow of the box [t] on the xy plane
       contains that of the point [p]. *)
  val overlaps_pt : t -> v3 -> bool

  (** [overlaps_box a b]

       Determine whether the projection/shadow of the box [a] on the xy plane
       overlaps that of the box [b]. *)
  val overlaps_box : t -> t -> bool

  (** [is_finite t]

       Determine whether the box [t] is finite. *)
  val is_finite : t -> bool

  (** {1 Booleans} *)

  (** [union a b]

       Return the union of the boxes [a] and [b] (enveloping the space of
       both). *)
  val union : t -> t -> t

  (** [include_pt t p]

       Expand the box [t] (mutating it) to include the point [p]. *)
  val include_pt : t -> v3 -> unit

  (** {1 Transformations} *)

  (** [translate p t]

       Move the box [t] along the vector [p]. *)
  val translate : v3 -> t -> t

  (** [scale factors t]

       Scale (multiply) the box [t] by the given xyz [factors]. *)
  val scale : v3 -> t -> t

  (** [transform m t]

       Transform box [t] with the affine transformation matrix [m]. *)
  val transform : Affine3.t -> t -> t

  (** {1 OCADml conversion} *)

  (** [of_bbox b]

       Construct a Manifold box from the bounds of the 3d OCADml bounding box [b]. *)
  val of_bbox : V3.bbox -> t

  (** [to_bbox t]

       Return the minimum and maximum bounds of the Manifold box [t] as an
       OCADml bounding box. *)
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

    (** [to_string t]

        Retrieve the corresponding label of the Manifold library's Error enum. *)
    val to_string : t -> string
  end

  module Id : sig
    type t =
      | Original of int
      | Product

    (** [to_int t]

          Return the plain integer ID. If the corresponding Manifold was a
          product, rather than an original, this returns [-1]. *)
    val to_int : t -> int
  end

  type t

  (** {1 Basic Constructors} *)

  (** [empty ()]

       Construct an empty Manifold. *)
  val empty : unit -> t

  (** [copy t]

       Return a copy of the Manifold [t]. *)
  val copy : t -> t

  (** [as_original t]

       If you copy a manifold, but you want this new copy to have new properties
       (e.g. a different UV mapping), you can reset its {!MeshRelation.BaryRef.mesh_id} to a new original,
       meaning it will now be referenced by its descendents instead of the meshes it
       was built from, allowing you to differentiate the copies when applying your
       properties to the final result.

       This function also condenses all coplanar faces in the relation, and
       collapses those edges. If you want to have inconsistent properties across
       these faces, meaning you want to preserve some of these edges, you should
       instead call GetMesh(), calculate your properties and use these to construct
       a new manifold. *)
  val as_original : t -> t

  (** [compose ts]
       Constructs a new Manifold from a list of other Manifolds. This is a purely
       topological operation, so care should be taken to avoid creating
       overlapping results. It is the inverse operation of {!decompose}. *)
  val compose : t list -> t

  (** [decompose t]

       This operation returns a list of Manifolds that are topologically
       disconnected. If everything is connected, the result is singular copy of
       the original. It is the inverse operation of {!compose}. *)
  val decompose : t -> t list

  (** {1 Data Extraction} *)

  (* TODO: with this doc in mind, I think I should just make result and exn
    versions of the of_mmesh/of_mesh functions and exclude this and status from
    the interface (just string result, so no need for user to convert it, or use
   variant/polyvariant still ?). *)

  (** [status t]
       Returns the reason for an input Mesh producing an empty Manifold. This Status
       only applies to Manifolds newly-created from an input {!MMesh.t} - once they are
       combined into a new Manifold via operations, the status reverts to NO_ERROR,
       simply processing the problem mesh as empty. Likewise, empty meshes may still
       show NO_ERROR, for instance if they are small enough relative to their
       precision to be collapsed to nothing. *)
  val status : t -> Status.t

  (** [original_id t]

       If this mesh is an original, this returns its
       {!MeshRelation.BaryRef.mesh_id} that can be referenced by product manifolds'
       {!MeshRelation.t}. *)
  val original_id : t -> Id.t

  (** [num_vert t]

       The number of vertices in the Manifold [t]. *)
  val num_vert : t -> int

  (** [num_edge t]

       The number of edges in the Manifold [t]. *)
  val num_edge : t -> int

  (** [num_tri t]

       The number of triangles in the Manifold [t]. *)
  val num_tri : t -> int

  (** [bounding_box t]

       Return the axis-aligned bounding box of all of the Manifold [t]'s
       vertices. *)
  val bounding_box : t -> Box.t

  (** [precision t]
       Returns the precision of this Manifold's vertices, which tracks the
       approximate rounding error over all the transforms and operations that have
       led to this state. Any triangles that are colinear within this precision are
       considered degenerate and removed. This is the value of &epsilon; defining
       {{:https://github.com/elalish/manifold/wiki/Manifold-Library#definition-of-%CE%B5-valid}
       &epsilon;-valid}. *)
  val precision : t -> float

  (* TODO: properties *)
  (* val properties : t -> Properties.t *)

  (** [genus t]

       The genus is a topological property of the manifold, representing the
       number of "handles". A sphere is 0, torus 1, etc. It is only meaningful
       for a single mesh, so it is best use {!decompose} first. *)
  val genus : t -> int

  (** [curvature t]

       Curvature is the inverse of the radius of curvature, and signed such that
       positive is convex and negative is concave. There are two orthogonal
       principal curvatures at any point on a manifold, with one maximum and the
       other minimum. Gaussian curvature is their product, while mean curvature is
       their sum. This approximates them for every vertex (returned as vectors in
       the structure) and also returns their minimum and maximum values. *)
  val curvature : t -> Curvature.t

  (** [mesh_relation t]

       Gets the relationship to the previous meshes, for the purpose of
       assigning properties like texture coordinates. The
       {!MeshRelation.BaryRef.tri_bary} vector is the same length as
       {!MMesh.points}: {!MeshRelation.BaryRef.original_id} indicates the source
       mesh and {!MeshRelation.BaryRef.tri} is that mesh's triangle index to which
       these barycentric coordinates refer. {!MeshRelation.BaryRef.vert_bary} gives
       an index for each vertex into the barycentric vector if that index is >= 0,
       indicating it is a new vertex. If the index is < 0, this indicates it is an
       original vertex, the index + 3 vert of the referenced triangle.

       {!MeshRelation.BaryRef.mesh_id} is a unique ID to the particular instance
       of a given mesh.  For instance, if you want to convert the triangle mesh to a
       polygon mesh, all the triangles from a given face will have the same
       [mesh_id] and [tri] values. *)
  val mesh_relation : t -> MeshRelation.t

  (** [points t]

       Retrieve the points making up the meshes of the Manifold [t]. *)
  val points : t -> v3 list

  (** {1 Shapes} *)

  val tetrahedron : unit -> t
  val sphere : ?fn:int -> float -> t
  val cube : ?center:bool -> v3 -> t
  val cylinder : ?center:bool -> ?fn:int -> height:float -> float -> t
  val cone : ?center:bool -> ?fn:int -> height:float -> float -> float -> t

  (** {1 Mesh Conversions} *)

  val of_mmesh : MMesh.t -> t
  val of_mesh : Mesh.t -> t
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
  val warp : (v3 -> v3) -> t -> t
  val scale : v3 -> t -> t
  val xscale : float -> t -> t
  val yscale : float -> t -> t
  val zscale : float -> t -> t
  val refine : int -> t -> t
  val hull : t list -> t

  (** {1 Quality Globals} *)

  (** [get_circular_segments t r]

       Determine how many segment there would be in a circle with radius [r],
       based on the governing parameters (set by {!set_circular_segments},
       {!set_min_circular_angle}, and {!set_min_circular_edge_length}). *)
  val get_circular_segments : float -> int

  val set_circular_segments : int -> unit
  val set_min_circular_angle : float -> unit
  val set_min_circular_edge_length : float -> unit
end

module Sdf2 : sig
  type t = OCADml.v2 -> float

  (** {1 Shapes} *)

  val circle : float -> t
  val square : ?round:float -> v2 -> t
  val rounded_box : ?tl:float -> ?tr:float -> ?bl:float -> ?br:float -> v2 -> t
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
