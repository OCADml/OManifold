open OCADml

type t = v3 -> float

let sphere r p = r -. V3.norm p

let cube ?round (dims : v3) =
  let dims =
    match round with
    | None -> V3.(dims /$ 2.)
    | Some r -> v3 ((dims.x /. 2.) -. r) ((dims.y /. 2.) -. r) ((dims.z /. 2.) -. r)
  in
  let f (p : v3) =
    let q = Float.(v3 (abs p.x -. dims.x) (abs p.y -. dims.y) (abs p.z -. dims.z)) in
    let len = Float.(V3.norm (v3 (max q.x 0.) (max q.y 0.) (max q.z 0.))) in
    Float.(neg @@ (len +. min (max q.x (max q.y q.z)) 0.))
  in
  match round with
  | None -> f
  | Some r -> fun p -> f p +. r

let torus (v : v2) (p : v3) =
  let q = v2 (V2.norm (v2 p.x p.z) -. v.x) p.y in
  Float.neg @@ (V2.norm q -. v.y)

let cylinder ?round ~height r =
  match round with
  | None ->
    let h = height /. 2. in
    fun (p : v3) ->
      let d = v2 (Float.abs p.z -. h) (Float.abs (V2.norm (v2 p.x p.y)) -. r) in
      Float.(neg (min (max d.x d.y) 0. +. V2.norm (v2 (max d.x 0.) (max d.y 0.))))
  | Some rnd ->
    let h = (height /. 2.) -. rnd in
    fun (p : v3) ->
      let d = v2 (Float.abs p.z -. h) (V2.norm (v2 p.x p.y) -. (r -. rnd)) in
      Float.(neg (min (max d.x d.y) 0. +. V2.norm (v2 (max d.x 0.) (max d.y 0.)) -. rnd))

let round r (t : t) p = t p +. r
let onion r (t : t) p = r -. Float.abs (t p)

let elongate h t =
  let hx = h.V3.x /. 2.
  and hy = h.y /. 2.
  and hz = h.z /. 2. in
  fun p ->
    let x = Float.abs p.V3.x -. hx
    and y = Float.abs p.y -. hy
    and z = Float.abs p.z -. hz in
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

let to_mmesh ?(level = 0.) ?(edge_length = 0.5) ~box sdf =
  let sdf x y z = sdf (v3 x y z) in
  let buf, mesh = MMesh.alloc ()
  and f =
    Ctypes.(coerce (Foreign.funptr C.Funcs.sdf_t) (static_funptr C.Funcs.sdf_t) sdf)
  in
  let _ = C.Funcs.level_set buf f box edge_length level in
  mesh

let to_mesh ?level ?edge_length ~box sdf =
  MMesh.to_mesh @@ to_mmesh ?level ?edge_length ~box sdf
