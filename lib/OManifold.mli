open OCADml

(**/**)

(* forward type declarations to allow mutually recursive use across modules and
    freedom to re-order appearance *)

type manifold
type mmeshgl
type cross_section

type op_type =
  [ `Add
  | `Subtract
  | `Intersect
  ]

(**/**)

(** {1 Constructive Solid Geometry} *)

module Cross : sig
  (** Two-dimensional cross sections guaranteed to be without self-intersections,
    or overlaps between polygons (from construction onwards) *)

  type t = cross_section

  (** Filling rules used by the clipping algorithm for boolean operations. See
       Clipper2's docs for a detailed
       {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/FillRule.htm}
       explanation} of how they differ). *)
  type fill_rule =
    [ `EvenOdd (** only odd numbered sub-regions are filled *)
    | `NonZero (** non-zero sub-regions are filled *)
    | `Positive (** only sub-regions with winding counts [> 0] are filled *)
    | `Negative (** only sub-regions with winding counts [< 0] are filled *)
    ]

  (** Defines the treatment of corners when offsetting paths. Visual examples
       are available in the Clipper2
       {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/JoinType.htm}
       docs}. *)
  type join_type =
    [ `Square
      (** squaring applied uniformally at all joins where the {i internal}
             join angle is less than 90 degrees. The squared edg will be at
             exactly the offset distance from the join vertex *)
    | `Round
      (** rounding is appliedto all joins that have convex external
             angles, and it maintains the exact offset distance from the join vertex *)
    | `Miter
      (** there's a necessary limit to mitered joins (to avoid narrow angled
              joins producing excessively long and narrow
              {{:http://www.angusj.com/clipper2/Docs/Units/Clipper.Offset/Classes/ClipperOffset/Properties/MiterLimit.htm}
              spikes})). The limit sets the maximum distance in multiples of
              the [delta] specified for the offsetting operation (default is
              [2.], which is the minimum allowed). *)
    ]

  (** {1 Constructors}*)

  (** [empty ()]

      Create an empty cross-section containing no contours. *)
  val empty : unit -> t

  (** [of_path ?fill_rule path]

       Create a 2d cross-section from a single outline [path]. A boolean union
       operation (with [`Positive] filling rule by default) is performed to ensure
       the resulting cross-section is free of self-intersections. *)
  val of_path : ?fill_rule:fill_rule -> Path2.t -> t

  (** [of_paths ?fill_rule paths]

       Create a 2d cross-section from a set of closed [paths] (zero or more
       complex polygons). A boolean union operation (with [`Positive] filling rule
       by default) is performed to combine overlapping polygons and ensure the
       resulting cross-section is free of intersections. *)
  val of_paths : ?fill_rule:fill_rule -> Path2.t list -> t

  (** [of_poly2 ?fill_rule poly]

      Create a 2d cross-section from an OCADml polygon. A boolean union
      operation (with [`Positive] filling rule by default) is performed to combine
      overlapping polygons and ensure the resulting cross-section is free of
      intersections. *)
  val of_poly2 : ?fill_rule:fill_rule -> Poly2.t -> t

  (** [of_poly2 ?fill_rule poly]

      Create a 2d cross-section from a list of OCADml polygons. A boolean union
      operation (with [`Positive] filling rule by default) is performed to combine
      overlapping polygons and ensure the resulting cross-section is free of
      intersections. *)
  val of_poly2s : ?fill_rule:fill_rule -> Poly2.t list -> t

  (** {1 Shapes} *)

  (** [circle ?fn r]

      Create a circular cross-section with radius [r]. [fn] can be provided to
      set the number of segments that it should be drawn with, otherwise it will
      be determined according to the globals governed by {!OManifold.Quality}. *)
  val circle : ?fn:int -> float -> t

  (** [square ?center dims]

       Create a square with the given XY [dims]. By default it is
       positioned in the first quadrant, touching the origin. Setting
       [~center:true] will instead centre the shape on the origin. If any
       dimensions in size are negative, or if all are zero, an empty {!Manifold.t}
       will be returned. *)
  val square : ?center:bool -> v2 -> t

  (** {1 Booleans} *)

  (** [boolean ~op a b]

      Compute the boolean operation [op] between the cross-sections [a] and
      [b]. *)
  val boolean : op:op_type -> t -> t -> t

  (** [batch_boolean ~op cs]

      Compute the boolean operation [op] on the list of cross-sections [cs]. In
      the case of [~op:`Subtract], the tail is differenced from the head. *)
  val batch_boolean : op:op_type -> t list -> t

  (** [add a b]

       Union (logical {b or}) the cross-sections [a] and [b]. *)
  val add : t -> t -> t

  (** [union ts]

       Union (logical {b or}) the list of cross-sections [ts]. *)
  val union : t list -> t

  (** [sub a b]

       Subtract (logical {b and not}) the cross-section [b] from the cross-section [a]. *)
  val sub : t -> t -> t

  (** [difference t d]

       Subtract (logical {b and not}) the list of cross-sections [d] from the cross-section [t]. *)
  val difference : t -> t list -> t

  (** [intersect a b]

       Compute the intersection (logical {b and}) of the cross-sections [a] and [b]. *)
  val intersect : t -> t -> t

  (** [intersection ts]

       Compute the intersection (logical {b and}) of the cross-sections [ts]. Only
       the area which is common or shared by {b all } shapes are retained. If
       [ts] is empty, an empty cross-section {!t} will result. *)
  val intersection : t list -> t

  (** {1 Topology} *)

  (** [compose ts]

      Create a cross-section from a list of cross-sections [ts] (see {!union}).
      Unlike {!Manifold.compose}, it isn't necessary that the input shapes are
      non-overlapping as a clipping operation is performed here. *)
  val compose : t list -> t

  (** [decompose t]

       Break the cross-section [t] into a list of topologically disconnected
       cross-sections, each containing one outline contour with zero or more
       holes. *)
  val decompose : t -> t list

  (** {1 Transformations} *)

  (** [translate p t]

       Move [t] along the vector [p]. *)
  val translate : v2 -> t -> t

  (** [xtrans x t]

    Move [t] by the distance [x] along the x-axis. *)
  val xtrans : float -> t -> t

  (** [ytrans y t]

    Move [t] by the distance [y] along the y-axis. *)
  val ytrans : float -> t -> t

  (** [rotate ?about r t]

    Rotate the cross-section [t] around the z-axis through the origin (or the point
    [about] if provided) by [r] (in radians). *)
  val rotate : ?about:v2 -> float -> t -> t

  (** [zrot ?about r t]

    Rotate the cross-section [t] around the z-axis through the origin (or the point
    [about] if provided) by [r] (in radians). *)
  val zrot : ?about:v2 -> float -> t -> t

  (** [mirror ax t]

       Mirror the cross-seciton [t] over the arbitrary axis described by the
       unit form of the vector [ax]. If the length of [ax] is zero, an empty
       cross-section is returned. *)
  val mirror : v2 -> t -> t

  (** [affine m t]

    Transform the cross-section [t] with the affine transformation matrix [m]. *)
  val affine : Affine2.t -> t -> t

  (** [scale factors t]

    Scales [t] by the given [factors] in xy. *)
  val scale : v2 -> t -> t

  (** [xscale s t]

    Scales [t] by the factor [s] in the x-dimension. *)
  val xscale : float -> t -> t

  (** [yscale s t]

    Scales [t] by the factor [s] in the y-dimension. *)
  val yscale : float -> t -> t

  (** [warp f t]

       Map over the vertices of the cross-section [t] with the function [f],
       followed by a union operation ensuring that the result is free of
       intersections. *)
  val warp : (v2 -> v2) -> t -> t

  (** {1 Path simplification and offsetting} *)

  (** [simplify ?eps t]

       Remove vertices from the contours in [t] that are less than
       the specified distance [eps] from an imaginary line that passes through
       its two adjacent vertices. Near duplicate vertices and collinear points
       will be removed at lower [eps]ilons, with elimination of line segments
       becoming increasingly aggressive with larger [eps]ilons.

       It is recommended to apply this function following {!offset}, in order to
       clean up any spurious tiny line segments introduced that do not improve
       quality in any meaningful way. This is particularly important if further
       offseting operations are to be performed, which would compound the
       issue. *)
  val simplify : ?eps:float -> t -> t

  (** [offset ?join_type ?miter_limit ?arc_tolerance ~delta t]

       Inflate the contours in the cross-section [t] by the [delta], handling
       corners according to [join_type] (default = [`Square]). Positive [delta]
       will cause the expansion of outlining contours to expand, and retraction of
       inner (hole) contours. Negative deltas will have the opposite effect.

       - [miter_limit] sets maximum distance in multiples of delta that vertices
         can be offset from their original positions with before squaring is
         applied, {b when the join type is [`Miter]} (default is [2.], which is the
         minimum allowed). See the {{:http://www.angusj.com/clipper2/Docs/Units/Clipper.Offset/Classes/ClipperOffset/Properties/MiterLimit.htm}Clipper2
         MiterLimit} page for a visual example.
       - [arc_tolerance] sets the maximum acceptable imperfection for curves
         drawn (approximated with line segments) for [`Round] joins (not relevant for
         other [join_type]s). By default the allowable imprecision is scaled in
         inverse proportion to the offset delta. *)
  val offset
    :  ?join_type:join_type
    -> ?miter_limit:float
    -> ?arc_tolerance:float
    -> delta:float
    -> t
    -> t

  (** {1 Geometry} *)

  (** [bounds t]

       Compute the axis-aligned bounding rectangle of all of the cross-section
       [t]'s vertices. *)
  val bounds : t -> Box2.t

  (** [area t]

       Compute the total area covered by complex polygons making up the
       cross-section [t]. *)
  val area : t -> float

  (** [num_vert t]

      The number of vertices in the cross-section [t]. *)
  val num_vert : t -> int

  (** [num_contour t]

      The number of contours in the cross-section [t]. *)
  val num_contour : t -> int

  (** [is_empty t]

      Does the cross-section [t] contain zero contours? *)
  val is_empty : t -> bool

  (** {1 Conversion} *)

  (** [to_paths t]

      Extract the contours (closed paths) from [t], describing zero or more
      complex polygons. If a list describing a single complex polygon with the
      its outline at the head, and inner/hole paths in the tail, one should first
      apply {!decompose}. *)
  val to_paths : t -> Path2.t list
end

module Manifold : sig
  (** Manifold triangular meshes representing 3d solid objects *)

  module Id : sig
    (** Unique identifier for a manifold {!type:t}, or a tag indicating that it
          is the result of transformations/operations on original/product manifolds *)
    type t =
      | Original of int
      | Product

    (** [to_int t]

          Return the plain integer ID. If the corresponding manifold was a
          product, rather than an original, this returns [-1]. *)
    val to_int : t -> int
  end

  type t = manifold

  (** {1 Basic Constructors} *)

  (** [empty ()]

       Construct an empty manifold. *)
  val empty : unit -> t

  (** [copy t]

       Return a copy of the manifold [t]. *)
  val copy : t -> t

  (** [as_original t]

       If you copy a manifold, but you want this new copy to have new properties
       (e.g. a different UV mapping), you can reset its relational ids (as found
       in {!MMeshGL.t} to new originals, meaning it will now be referenced by its
       descendents instead of the meshes it was built from, allowing you to
       differentiate the copies when applying your properties to the final result.

       This function also condenses all coplanar faces in the relation, and
       collapses those edges. If you want to have inconsistent properties across
       these faces, meaning you want to preserve some of these edges, you should
       instead use {!to_mmeshgl}, calculate your properties and use these to construct
       a new manifold. *)
  val as_original : t -> t

  (** {1 Topology} *)

  (** [compose ts]
       Constructs a new manifold from a list of other manifolds. This is a purely
       topological operation, so care should be taken to avoid creating
       overlapping results. It is the inverse operation of {!decompose}. *)
  val compose : t list -> t

  (** [decompose t]

       This operation returns a list of manifolds that are topologically
       disconnected. If everything is connected, the result is singular copy of
       the original. It is the inverse operation of {!compose}. *)
  val decompose : t -> t list

  (** {1 Shapes} *)

  (** [tetrahedron ()]

       Create a tetrahedron centred at the origin with one vertex at
       [(v3 1. 1. 1.)] and the rest at similarly symmetric points. *)
  val tetrahedron : unit -> t

  (** [sphere ?fn radius]

       Create a sphere with given [radius] at the origin of the coordinate
       system. The number of segments along the diameter can be explicitly set by
       [fn], otherwise it is determined by the {{!OManifold.Quality} quality globals}. *)
  val sphere : ?fn:int -> float -> t

  (** [cube ?center dimensions]

    Create a cube in the first octant, with the given xyz [dimensions]. When
    [center] is true, the cube is centered on the origin. *)
  val cube : ?center:bool -> v3 -> t

  (** [cylinder ?center ?fn ~height radius]

     Creates a cylinder centered about the z-axis. When [center] is true, it will
     also be centered vertically, otherwise the base will sit upon the XY
     plane. The number of segments along the diameter can be explicitly set by
     [fn], otherwise it is determined by the {{!OManifold.Quality} quality globals}. *)
  val cylinder : ?center:bool -> ?fn:int -> height:float -> float -> t

  (** [cone ?center ?fn ~height r1 r2 ]

     Create cone with bottom radius [r1] and top radius [r2]. When [center] is
     true, it will also be centered vertically, otherwise the base will sit upon
     the XY plane. The number of segments along the diameter can be explicitly set by
     [fn], otherwise it is determined by the {{!OManifold.Quality} quality globals}.*)
  val cone : ?center:bool -> ?fn:int -> height:float -> float -> float -> t

  (** {1 Mesh Conversions} *)

  (** [of_mmeshgl m]

       Create a manifold from the mesh [m], returning [Error] if [m] is not an
       oriented 2-manifold. Will collapse degenerate triangles and unnecessary
       vertices. *)
  val of_mmeshgl : mmeshgl -> (t, string) result

  (** [of_mmeshgl_exn ?properties m]

       Same as {!of_mmeshgl}, but raising a [Failure] rather than returning an
       [Error]. *)
  val of_mmeshgl_exn : mmeshgl -> t

  (** [of_mesh ?rev m]

       Create a manifold from an OCADml mesh [m], returning [Error] if [m] is
       not an oriented 2-manifold. Will collapse degenerate triangles and
       unnecessary vertices. If [rev] is [true] (as it is by default), faces of
       the input mesh are reversed (as the winding convention in OCADml is opposite
       to Manifold) *)
  val of_mesh : ?rev:bool -> Mesh.t -> (t, string) result

  (** [of_mesh_exn ?rev m]

       Same as {!of_mesh}, but raising a [Failure] rather than returning an
       [Error]. *)
  val of_mesh_exn : ?rev:bool -> Mesh.t -> t

  (** [smooth ?smoothness m]

       Constructs a smooth version of the input mesh [m] by creating tangents,
       returning an error if you have already supplied tangents for it. The actual
       triangle resolution is unchanged, thus you will likely want to follow up
       with {!refine} to interpolate to higher-resolution curves.

       By default, every edge is calculated for maximum smoothness (very much
       approximately), attempting to minimize the maximum mean Curvature magnitude.
       No higher-order derivatives are considered, as the interpolation is
       independent per triangle, only sharing constraints on their boundaries.
       To control the relative smoothness at particular halfedges (ideally
       limited to a small subset of all halfedges), [smoothness] can be provided
       with [(index, s)] pairs specifying a smoothness factor [s] between [0.]
       and [1.] for the [index] interperpreted as [3 * triangle + {0,1,2}] where
       [0] is the edge between the first and second vertices of the [triangle].

       At a smoothness value of zero, a sharp crease is made. The smoothness is
       interpolated along each edge, so the specified value should be thought of as
       an average. Where exactly two sharpened edges meet at a vertex, their
       tangents are rotated to be colinear so that the sharpened edge can be
       continuous. Vertices with only one sharpened edge are completely smooth,
       allowing sharpened edges to smoothly vanish at termination. A single vertex
       can be sharpened by sharping all edges that are incident on it, allowing
       cones to be formed. *)
  val smooth : ?smoothness:(int * float) list -> MMeshGL.t -> (t, string) result

  (** [smooth_exn ?smoothness m]

       Same as {!smooth}, but raising [Failure] rather than returning [Error]. *)
  val smooth_exn : ?smoothness:(int * float) list -> MMeshGL.t -> t

  (** [to_mmeshgl t]

       Obtain a graphics library (gl) friendly mesh representation of the manifold [t]. *)
  val to_mmeshgl : t -> mmeshgl

  (** [to_mesh t]

       Obtain an OCADml mesh describing the shape ot the manifold [t]. *)
  val to_mesh : t -> Mesh.t

  (** {1 2D to 3D} *)

  (** [extrude ?slices ?twist ?scale ~height cross_section]

    Vertically extrude a 2d [cross_section] from the XY plane to
    [height]. If [?center] is [true], the resulting 3D object is centered around
    the XY plane, rather than resting on top of it.
    - [?twist] rotates the shape by the specified angle (in radians) as it is
      extruded upwards
    - [?slices] specifies the number of intermediate points along the Z axis of
      the extrusion. By default this increases with the value of [?twist],
      though manual refinement may improve results.
    - [?scale] expands or contracts the shape in X and Y as it is extruded
      upward. Default is [(v2 1. 1.)], no scaling. If set to [(v2 0. 0.)], a
      pure cone is formed, with only a single vertex at the top. *)
  val extrude
    :  ?slices:int
    -> ?fa:float
    -> ?twist:float
    -> ?scale:v2
    -> ?center:bool
    -> height:float
    -> Cross.t
    -> t

  (** [revolve ?fn ?angle cross_section]

       Revolve a 2d [cross_section] around the y-axis and then set this
       as the z-axis of the resulting manifold. If the polygons cross the y-axis, only
       the part on the positive x side is used. Geometrically valid input will result
       in geometrically valid output. The number of segments in the revolution
       can be set explicitly with [fn], otherwise it is determined by the
       {{!OManifold.Quality} quality globals}. A full revolution is performed
       by default, but it can be made partial by providing an [angle] in radians. *)
  val revolve : ?fn:int -> ?angle:float -> Cross.t -> t

  (** {1 Booleans} *)

  val boolean : op:op_type -> t -> t -> t
  val batch_boolean : op:op_type -> t list -> t

  (** [add a b]

       Union (logical {b or}) the manifolds [a] and [b]. *)
  val add : t -> t -> t

  (** [union ts]

       Union (logical {b or}) the list of manifolds [ts]. *)
  val union : t list -> t

  (** [sub a b]

       Subtract (logical {b and not}) the manifold [b] from the manifold [a]. *)
  val sub : t -> t -> t

  (** [difference t d]

       Subtract (logical {b and not}) the list of manifolds [d] from the manifold [t]. *)
  val difference : t -> t list -> t

  (** [intersect a b]

       Compute the intersection (logical {b and}) of the manifolds [a] and [b]. *)
  val intersect : t -> t -> t

  (** [intersection ts]

       Compute the intersection (logical {b and}) of the manifolds [ts]. Only
       the area which is common or shared by {b all } shapes are retained. If
       [ts] is empty, an empty manifold {!t} will result. *)
  val intersection : t list -> t

  (** [split a b]

       Splits the manifold [a] into two using the cutter manifold [b]. The
       first result is the intersection, and the second is the difference. *)
  val split : t -> t -> t * t

  (** [split_by_plane p t]

       Splits the manifold [t] in two, one above the plane [p], and the other below. *)
  val split_by_plane : Plane.t -> t -> t * t

  (** [trim_by_plane p t]

       Cut away the portion of the manifold [t] lying below the plane [p]. *)
  val trim_by_plane : Plane.t -> t -> t

  (** {1 Transformations} *)

  (** [translate p t]

       Move [t] along the vector [p]. *)
  val translate : v3 -> t -> t

  (** [xtrans x t]

    Move [t] by the distance [x] along the x-axis. *)
  val xtrans : float -> t -> t

  (** [ytrans y t]

    Move [t] by the distance [y] along the y-axis. *)
  val ytrans : float -> t -> t

  (** [ztrans z t]

    Move [t] by the distance [z] along the z-axis. *)
  val ztrans : float -> t -> t

  (** [rotate ?about r t]

    Performs an Euler rotation (zyx). If it is provided, rotations are performed
    around the point [about], otherwise rotation is about the origin. Angle(s)
    [r] are in radians. *)
  val rotate : ?about:v3 -> v3 -> t -> t

  (** [xrot ?about r t]

    Rotate the manifold [t] around the x-axis through the origin (or the point
    [about] if provided) by [r] (in radians). *)
  val xrot : ?about:v3 -> float -> t -> t

  (** [yrot ?about r t]

    Rotate the manifold [t] around the y-axis through the origin (or the point
    [about] if provided) by [r] (in radians). *)
  val yrot : ?about:v3 -> float -> t -> t

  (** [zrot ?about r t]

    Rotate the manifold [t] around the z-axis through the origin (or the point
    [about] if provided) by [r] (in radians). *)
  val zrot : ?about:v3 -> float -> t -> t

  (** [mirror n t]

       Mirror the manifold [t] over the plane described by the unit form of the
       normal vector [n]. If the length of [n] is zero, an empty manifold is
       returned. *)
  val mirror : v3 -> t -> t

  (** [affine m t]

    Transform the manifold [t] with the affine transformation matrix [m]. *)
  val affine : Affine3.t -> t -> t

  (** [quaternion ?about q t]

    Applys the quaternion rotation [q] around the origin (or the point [about]
    if provided) to [t]. *)
  val quaternion : ?about:v3 -> Quaternion.t -> t -> t

  (** [axis_rotate ?about ax r t]

    Rotates [t] about the arbitrary axis [ax] through the origin (or the point
    [about] if provided) by the angle [r] (in radians). *)
  val axis_rotate : ?about:v3 -> v3 -> float -> t -> t

  (** [warp f t]

       Map over the vertices of the manifold [t] with the function [f], allowing
       their positions to be updated arbitrarily, but note that the topology is
       unchanged. It is easy to create a function that warps a geometrically valid
       object into one which overlaps, but that is not checked here, so it is up to
       the user to choose their function with discretion. *)
  val warp : (v3 -> v3) -> t -> t

  (** [scale factors t]

    Scales [t] by the given [factors] in xyz. *)
  val scale : v3 -> t -> t

  (** [xscale s t]

    Scales [t] by the factor [s] in the x-dimension. *)
  val xscale : float -> t -> t

  (** [yscale s t]

    Scales [t] by the factor [s] in the y-dimension. *)
  val yscale : float -> t -> t

  (** [zscale s t]

    Scales [t] by the factor [s] in the z-dimension. *)
  val zscale : float -> t -> t

  (** [refine n t]

       Increase the density of the meshes in [t] by splitting every edge into
       [n] pieces. For instance, with [n = 2], each triangle will be split into 4
       triangles. These will all be coplanar (and will not be immediately collapsed)
       unless the contained {!MMeshGL.t} within has {!MMeshGL.halfedge_tangents}
       specified (e.g. from the {!smooth} constructor), in which case the new
       vertices will be moved to the interpolated surface according to their
       barycentric coordinates. *)
  val refine : int -> t -> t

  (** [hull ts]

       Create a convex hull that encloses all of the vertices of the manifolds
       in [ts]. Note that this operation comes from OCADml and is not guaranteed
       to produce a valid manifold. *)
  val hull : t list -> (t, string) result

  (** [hull_exn ts]

       Same as {!hull}, but a [Failure] is raised if the resulting mesh does
       not describe a valid manifold. *)
  val hull_exn : t list -> t

  (** {1 Data Extraction} *)

  (** [original_id t]

       If this mesh is an original, this returns it unique id that can be
       referenced by product manifolds (for the purposes of reappling mesh properties). *)
  val original_id : t -> Id.t

  (** [num_vert t]

       The number of vertices in the manifold [t]. *)
  val num_vert : t -> int

  (** [num_edge t]

       The number of edges in the manifold [t]. *)
  val num_edge : t -> int

  (** [num_tri t]

       The number of triangles in the manifold [t]. *)
  val num_tri : t -> int

  (** [bounding_box t]

       Return the axis-aligned bounding box of all of the manifold [t]'s
       vertices. *)
  val bounding_box : t -> Box3.t

  (** [epsilon t]

       Returns the precision of this manifold's vertices, which tracks the
       approximate rounding error over all the transforms and operations that have
       led to this state. Any triangles that are colinear within this precision are
       considered degenerate and removed. This is the value of &epsilon; defining
       {{:https://github.com/elalish/manifold/wiki/manifold-Library#definition-of-%CE%B5-valid}
       &epsilon;-valid}. *)
  val epsilon : t -> float

  (** [genus t]

       The genus is a topological property of the manifold, representing the
       number of "handles". A sphere is 0, torus 1, etc. It is only meaningful
       for a single mesh, so it is best use {!decompose} first. *)
  val genus : t -> int

  (** [surface_area t]

       The surface area of the manifold [t]. *)
  val surface_area : t -> float

  (** [volume t]

       The volume of the manifold [t]. *)
  val volume : t -> float

  (** [points t]

       Retrieve the points making up the meshes of the manifold [t]. *)
  val points : t -> v3 list
end

module Quality : sig
  (** Quality globals akin to {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#$fa,_$fs_and_$fn}OpenSCAD}'s facet governing "special"
       parameters, [$fn], [$fa], and [$fs]. These are global variables that are
       handy for quickly switching from quick and dirty rough iteration
       quality to computationally expensive smooth final products. *)

  (** [get_circular_segments r]

       Determine how many segment there would be in a circle with radius [r],
       based on the global quality parameters (set by {!set_circular_segments},
       {!set_min_circular_angle}, and {!set_min_circular_edge_length}). *)
  val get_circular_segments : float -> int

  (** [set_circular_segments fn]

       Set a default number of segments that circular shape are drawn with (by
       default this is unset, and minimum circular angle and length are used to
       calculate the number of segments instead). This takes precedence over both
       minumum circular angle and edge length, and is akin to defining the special
       [$fn] variable at the top level of an {{:https://openscad.org/}OpenSCAD} script. *)
  val set_circular_segments : int -> unit

  (** [set_min_circular_angle fa]

       Change the default minimum angle (in radians) between consecutive segments on a
       circular object/edge (default is [pi /. 18.]). This is akin to setting
       the special [$fa] variable at the top level of an
       {{:https://openscad.org/}OpenSCAD} script. *)
  val set_min_circular_angle : float -> unit

  (** [set_min_circular_edge_length fs]

       Change the default minimum edge length for segments that that circular
       objects/edges are drawn with (default = [1.]). This is akin to setting
       the special [$fs] variable at the top level of an
       {{:https://openscad.org/}OpenSCAD} script. *)
  val set_min_circular_edge_length : float -> unit
end

(** {1 Signed Distance Functions}

Manifold provides a level set algorithm for the generation of manifold meshes
from {{:https://en.wikipedia.org/wiki/Signed_distance_function} signed distance
functions} (positive inside, negative outside). Unfortunately due to the OCaml
runtime lock, only sequential (without multiprocessing or GPU acceleration)
execution of sdfs defined here are possible, thus performance will be slower
than using {{:https://github.com/elalish/manifold} Manifold} or another SDF CAD
library such as {{:https://github.com/libfive/libfive} libfive} directly. *)

module Sdf2 : sig
  (** 2d signed-distance functions (extrudable to 3d) *)

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
  (** 3d signed-distance functions (convertible to mesh/manifold via a level
    set algorith) *)

  (** A negative inside, positive outside signed-distance function. *)
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

  (** [to_mmeshgl ?level ?edge_length ~box t]

       Constructs a level-set Mesh from the signed-distance function [t].
       This uses a form of Marching Tetrahedra (akin to Marching Cubes, but better
       for manifoldness). Instead of using a cubic grid, it uses a body-centered
       cubic grid (two shifted cubic grids). This means if your function's interior
       exceeds the given bounds, you will see a kind of egg-crate shape closing off
       the manifold, which is due to the underlying grid. The output is
       guaranteed to be manifold, thus should always be an appropriate input to
       {!Manifold.of_mmeshgl}.

      - [box] is and axis-aligned bounding box defining the extent of the grid
        overwhich [t] is evaluated
      - [edge_length] is the the approximate maximum edge length of the triangles
        in the final result. This affects grid spacing, thus strongly impacting performance.
      - [level] can be provided with a positive value to inset the mesh, or a
        negative value to outset it (default is [0.] -- no offset)
      - [tolerance] TODO: description *)
  val to_manifold
    :  ?level:float
    -> ?edge_length:float
    -> ?tolerance:float
    -> box:Box3.t
    -> t
    -> manifold
end

(** {1 IO} *)

module MMeshGL : sig
  (** A graphics library friendly representation of manifold's internal mesh.
       Obtained via {!Manifold.to_mmeshgl}, or constructed directly with {!MMeshGL.make} *)

  type t = mmeshgl

  (** {1 Constructor} *)

  (** [make ?normals ?tangents points triangles]

       Create a manifold mesh from a set of [points], and [triangles]
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

  val num_prop : t -> int
  val num_vert : t -> int
  val num_tri : t -> int
  val properties : t -> float list
  val triangles : t -> int list

  (** [halfedge_tangents t]

       Retrieve the halfedge tangents of the mesh [t] (three for each of the
       [t]'s {!triangles}). *)
  val halfedge_tangents : t -> float list

  val run_index : t -> int list
  val run_original_id : t -> int list
  val run_transform : t -> float list
  val face_id : t -> int list

  (** [points t]

       Retrieve the points making up the mesh [t]. *)
  val points : t -> v3 list

  (** [faces t]

       Retrieve the triangular faces of the mesh [t] as triples of indices into
       its {!points}. *)
  val faces : t -> Mesh.tri list

  (** {1 OCADml conversion}

      To and from the OCADml [Mesh.t] type. As [Mesh.t] follows the OpenSCAD
      winding convention (opposite to manifold) the triangles/faces are
      reversed. *)

  (** [of_mesh ?rev m]

       Create a manifold mesh from the [OCADml.Mesh.t] [m]. If [rev] is [true]
       (as it is by default), faces of the input mesh are reversed (as the
       winding convention in OCADml is opposite to Manifold) *)
  val of_mesh : ?rev:bool -> Mesh.t -> t

  (** [to_mesh t]

       Create an [OCADml.Mesh.t] from the manifold mesh [t].  *)
  val to_mesh : t -> Mesh.t

  (** {1 Import} *)

  (** [import ?force_cleanup path]

      Load a mesh from disk at the given [path] using Manifold's MeshIO wrapper
      over assimp. If [cleanup] is [true] (defaults to [false]), identical
      vertices in the input will be merged to promote manifoldness. *)
  val import : ?cleanup:bool -> string -> t
end

module Export : sig
  (** Writing manifold meshes to disk via {{:https://github.com/assimp/assimp}
    Open Asset Import Library (assimp)} *)

  module Material : sig
    type t

    (* TODO: reimpl, vert_color is gone *)
    (* val make *)
    (*   :  ?roughness:float *)
    (*   -> ?metalness:float *)
    (*   -> ?color:v4 *)
    (*   -> ?vert_color:v4 list *)
    (*   -> unit *)
    (*   -> t *)
  end

  module Opts : sig
    (** Export options *)
    type t

    val make : ?faceted:bool -> ?material:Material.t -> unit -> t
  end

  val mmeshgl : ?opts:Opts.t -> string -> MMeshGL.t -> unit
  val manifold : ?opts:Opts.t -> string -> Manifold.t -> unit
end
