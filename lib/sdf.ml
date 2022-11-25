open OCADml

type t = v3 -> float

let sphere r p = r -. V3.norm p

let cube ?(round = 0.) (dims : v3) (p : v3) =
  let q = Float.(v3 (abs p.x -. dims.x) (abs p.y -. dims.y) (abs p.z -. dims.z)) in
  let len = Float.(V3.norm (v3 (max q.x 0.) (max q.y 0.) (max q.z 0.))) in
  Float.neg @@ (len +. Float.(min (max q.x (max q.y q.z)) 0.) -. round)

let torus (v : v2) (p : v3) =
  let q = v2 (V2.norm (v2 p.x p.z) -. v.x) p.y in
  Float.neg @@ (V2.norm q -. v.y)

let cylinder ?round ~height r =
  match round with
  | None ->
    fun (p : v3) ->
      let d = v2 (Float.abs (V2.norm (v2 p.x p.z)) -. height) (Float.abs p.y -. r) in
      Float.(neg (min (max d.x d.y) 0. +. V2.norm (v2 (max d.x 0.) (max d.y 0.))))
  | Some rnd ->
    fun (p : v3) ->
      let d = v2 (V2.norm (v2 p.x p.z) -. (2. *. r) +. rnd) (Float.abs p.y -. height) in
      Float.(neg (min (max d.x d.y) 0. +. V2.norm (v2 (max d.x 0.) (max d.y 0.))) -. rnd)

let round r t p = t p +. r

let elongate (h : v3) t (p : v3) =
  let x = Float.abs p.x -. h.x
  and y = Float.abs p.y -. h.y
  and z = Float.abs p.z -. h.z in
  let w = Float.(min (max x (max y z)) 0.) in
  t Float.(v3 (max x 0. +. w) (max y 0. +. w) (max z 0. +. w))

let translate v t p = t (V3.sub p v)

let rotate ?about r t =
  match about with
  | Some abt -> fun p -> t V3.(rotate ~about:(neg abt) (neg r) p)
  | None -> fun p -> t V3.(rotate (neg r) p)

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
