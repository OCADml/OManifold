open OCADml

type t = v2 -> float

let circle r p = r -. V2.norm p

let square ?round dims =
  let dims =
    match round with
    | None -> V2.(dims /$ 2.)
    | Some r -> v2 ((dims.x /. 2.) -. r) ((dims.y /. 2.) -. r)
  in
  let f p =
    let dx = Float.abs p.V2.x -. dims.V2.x
    and dy = Float.abs p.y -. dims.y in
    Float.(neg @@ (V2.norm (v2 (max dx 0.) (max dy 0.)) +. min (max dx dy) 0.))
  in
  match round with
  | None -> f
  | Some r -> fun p -> f p +. r

let rounded_box ?(tl = 0.) ?(tr = 0.) ?(bl = 0.) ?(br = 0.) dims =
  let dx = dims.V2.x /. 2.
  and dy = dims.y /. 2. in
  fun p ->
    let r =
      match p.V2.x > 0., p.y > 0. with
      | true, true -> tr
      | true, false -> br
      | false, true -> tl
      | false, false -> bl
    in
    let q = v2 (Float.abs p.x -. dx +. r) (Float.abs p.y -. dy +. r) in
    Float.(neg @@ (min (max q.x q.y) 0. +. V2.norm (v2 (max q.x 0.) (max q.y 0.)) -. r))

let rhombus ?round dims =
  let dims =
    match round with
    | None -> V2.(dims *$ 0.5)
    | Some r -> v2 ((dims.x *. 0.5) -. r) ((dims.y *. 0.5) -. r)
  and ndot a b = (a.V2.x *. b.V2.x) -. (a.y *. b.y) in
  let f p =
    let p = v2 (Float.abs p.x) (Float.abs p.y) in
    let h =
      Math.clamp ~min:(-1.) ~max:1. V2.(ndot (dims -@ (p *$ 2.)) dims /. dot dims dims)
    in
    let d =
      V2.(norm (p -@ v2 ((1. -. h) *. dims.x *. 0.5) ((1. +. h) *. dims.y *. 0.5)))
    in
    Float.neg
    @@ (d *. Math.sign ((p.x *. dims.y) +. (p.y *. dims.x) -. (dims.x *. dims.y)))
  in
  match round with
  | None -> f
  | Some r -> fun p -> f p +. r

let round r (t : t) p = t p +. r
let onion r (t : t) p = r -. Float.abs (t p)

let elongate h t =
  let hx = h.V2.x /. 2.
  and hy = h.y /. 2. in
  fun p ->
    let x = Float.abs p.V2.x -. hx
    and y = Float.abs p.y -. hy in
    let w = Float.(min (max x y)) 0. in
    t Float.(v2 (max x 0. +. w) (max y 0. +. w))

let translate v t p = t (V2.sub p v)
let[@inline] xtrans x t p = translate (v2 x 0.) t p
let[@inline] ytrans y t p = translate (v2 0. y) t p

let rotate ?about r t =
  match about with
  | Some abt -> fun p -> t V2.(rotate ~about:(neg abt) (-.r) p)
  | None -> fun p -> t V2.(rotate (-.r) p)

let[@inline] zrot ?about r t p = rotate ?about r t p
let scale s t p = t V2.(p /$ s) *. s

let extrude ~height t p =
  let wx = Float.neg @@ t (v2 p.V3.x p.y)
  and wy = Float.abs p.z -. (height /. 2.) in
  Float.neg @@ (min (max wx wy) 0. +. V2.norm (v2 (max wx 0.) (max wy 0.)))

let revolve ?offset t =
  match offset with
  | None -> fun p -> t @@ v2 (V2.norm (v2 p.V3.x p.z)) p.y
  | Some o -> fun p -> t @@ v2 (V2.norm (v2 p.V3.x p.z) -. o) p.y
