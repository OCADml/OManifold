open OCADml

module type MRect = sig
  (** The 2d axis-aligned recantagle (bounding box) representation of
      the Manifold library *)

  type t

  (** {1 Constructor} *)

  (** [make a b]

       Create a bounding box that contains the points [a] and [b]. *)
  val make : v2 -> v2 -> t

  (** {1 Data / Queries} *)

  (** [min t]

       The minimum bound of the box [t]. *)
  val min : t -> v2

  (** [max t]

       The maximum bound of the box [t]. *)
  val max : t -> v2

  (** [center t]

       The center point of the box [t]. *)
  val center : t -> v2

  (** [dimensions t]

       The 2-dimensional extent (xyz) of the box [t]. *)
  val dimensions : t -> v2

  (** [abs_max_coord t]

       The absolute largest dimensional coordinate of the box [t]. *)
  val abs_max_coord : t -> float

  (** [contains_pt t p]

       Determine whether the box [t] contains the point [p]. *)
  val contains_pt : t -> v2 -> bool

  (** [contains_rect a b]

       Determine whether the box [a] contains the box [b]. *)
  val contains_rect : t -> t -> bool

  (** [overlaps_rect a b]

       Determine whether the rectangles [a] and [b] overlap/intersect. *)
  val overlaps_rect : t -> t -> bool

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
  val include_pt : t -> v2 -> unit

  (** {1 Transformations} *)

  (** [translate p t]

       Move the box [t] along the vector [p]. *)
  val translate : v2 -> t -> t

  (** [scale factors t]

       Scale (multiply) the box [t] by the given xy [factors]. *)
  val scale : v2 -> t -> t

  (** [transform m t]

       Transform box [t] with the affine transformation matrix [m]. *)
  val transform : Affine2.t -> t -> t

  (** {1 Conversion} *)

  (** [to_cross_section t]

       Construct a cross section from the rectangle [t]. *)
  val to_cross_section : t -> CrossSection.t

  (** {2 OCADml} *)

  (** [of_box b]

       Construct a manifold box from the bounds of the 2d Gg/OCADml bounding box [b]. *)
  val of_box : Box2.t -> t

  (** [to_box t]

       Return the minimum and maximum bounds of the manifold box [t] as an
       Gg/OCADml bounding box. *)
  val to_box : t -> Box2.t
end

module type MBox = sig
  (** The 3d axis-aligned bounding box representation of the Manifold library *)

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

  (** [of_box b]

       Construct a manifold box from the bounds of the 3d Gg/OCADml bounding box [b]. *)
  val of_box : Box3.t -> t

  (** [to_box t]

       Return the minimum and maximum bounds of the manifold box [t] as an
       Gg/OCADml bounding box. *)
  val to_box : t -> Box3.t
end
