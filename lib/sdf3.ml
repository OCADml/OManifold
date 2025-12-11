open OCADml

type t = v3 -> float

let sphere r p = r -. V3.norm p

let cube ?round (dims : v3) =
  let dims =
    match round with
    | None -> V3.(dims /$ 2.)
    | Some r -> V3.(v ((x dims /. 2.) -. r) ((y dims /. 2.) -. r) ((z dims /. 2.) -. r))
  in
  let f (p : v3) =
    let q =
      V3.(
        v
          (Float.abs (x p) -. x dims)
          (Float.abs (y p) -. y dims)
          (Float.abs (z p) -. z dims) )
    in
    let len =
      Float.(V3.norm (v3 (max (V3.x q) 0.) (max (V3.y q) 0.) (max (V3.z q) 0.)))
    in
    Float.(neg @@ (len +. min (max (V3.x q) (max (V3.y q) (V3.z q))) 0.))
  in
  match round with
  | None -> f
  | Some r -> fun p -> f p +. r

let torus (v : v2) (p : v3) =
  let q = v2 (V2.norm (v2 (V3.x p) (V3.z p)) -. V2.x v) (V3.y p) in
  Float.neg @@ (V2.norm q -. V2.y v)

let cylinder ?round ~height r =
  match round with
  | None ->
    let h = height /. 2. in
    fun p ->
      let d =
        v2 (Float.abs (V3.z p) -. h) (Float.abs (V2.norm (v2 (V3.x p) (V3.y p))) -. r)
      in
      Float.(neg V2.(min (max (x d) (y d)) 0. +. norm (v2 (max (x d) 0.) (max (y d) 0.))))
  | Some rnd ->
    let h = (height /. 2.) -. rnd in
    fun p ->
      let d = V3.(v2 (Float.abs (z p) -. h) (V2.norm (v2 (x p) (y p)) -. (r -. rnd))) in
      Float.(
        neg V2.(min (max (x d) (y d)) 0. +. norm (v2 (max (x d) 0.) (max (y d) 0.)) -. rnd) )

let round r (t : t) p = t p +. r
let onion r (t : t) p = r -. Float.abs (t p)

let elongate h t =
  let hx = V3.x h /. 2.
  and hy = V3.y h /. 2.
  and hz = V3.z h /. 2. in
  fun p ->
    let x = Float.abs (V3.x p) -. hx
    and y = Float.abs (V3.y p) -. hy
    and z = Float.abs (V3.z p) -. hz in
    let w = Float.(min (max x (max y z)) 0.) in
    t Float.(v3 (max x 0. +. w) (max y 0. +. w) (max z 0. +. w))

let translate v t p = t (V3.sub p v)
let[@inline] xtrans x t p = translate (v3 x 0. 0.) t p
let[@inline] ytrans y t p = translate (v3 0. y 0.) t p
let[@inline] ztrans z t p = translate (v3 0. 0. z) t p

let rotate ?about r t =
  match about with
  | Some abt -> fun p -> t V3.(rotate ~about:(neg abt) (neg r) p)
  | None -> fun p -> t V3.(rotate (neg r) p)

let[@inline] xrot ?about x t p = rotate ?about (v3 x 0. 0.) t p
let[@inline] yrot ?about y t p = rotate ?about (v3 0. y 0.) t p
let[@inline] zrot ?about z t p = rotate ?about (v3 0. 0. z) t p

let axis_rotate ?about ax r t =
  match about with
  | Some abt -> fun p -> t V3.(axis_rotate ~about:(neg abt) ax (-.r) p)
  | None -> fun p -> t (V3.axis_rotate ax (-.r) p)

let quaternion ?about q t =
  match about with
  | Some abt -> fun p -> t Quaternion.(transform ~about:(V3.neg abt) (conj q) p)
  | None -> fun p -> t Quaternion.(transform (conj q) p)

let scale s t p = t V3.(p /$ s) *. s

let union ?smooth a b =
  match smooth with
  | None -> fun p -> Float.max (a p) (b p)
  | Some k ->
    fun p ->
      let da = a p
      and db = b p in
      let h = Math.clamp ~min:0. ~max:1. ((0.5 -. (0.5 *. (da -. db))) /. k) in
      Math.lerp da db h +. (k *. h *. (1. -. h))

let difference ?smooth a b =
  match smooth with
  | None -> fun p -> Float.min (a p) (-1. *. b p)
  | Some k ->
    fun p ->
      let da = a p
      and db = b p in
      let h = Math.clamp ~min:0. ~max:1. ((0.5 +. (0.5 *. (da +. db))) /. k) in
      Math.lerp da (-.db) h -. (k *. h *. (1. -. h))

let intersection ?smooth a b =
  match smooth with
  | None -> fun p -> Float.min (a p) (b p)
  | Some k ->
    fun p ->
      let da = a p
      and db = b p in
      let h = Math.clamp ~min:0. ~max:1. ((0.5 +. (0.5 *. (da -. db))) /. k) in
      Math.lerp da db h -. (k *. h *. (1. -. h))

let to_manifold ?(level = 0.) ?(edge_length = 0.5) ?(tolerance = 1e-3) ~box sdf =
  let sdf x y z _ = sdf (v3 x y z) in
  let buf, man = Manifold.alloc ()
  and f = Ctypes.(coerce (Foreign.funptr C.Funcs.sdf_t) (static_funptr C.Funcs.sdf_t) sdf)
  and box = MBox.of_box box in
  let _ = C.Funcs.level_set_seq buf f box edge_length level tolerance Ctypes.null in
  man

let to_mesh ?level ?edge_length ~box sdf =
  Manifold.to_mesh @@ to_manifold ?level ?edge_length ~box sdf
