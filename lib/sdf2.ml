open OCADml

type t = v2 -> float

let circle r p = r -. V2.norm p

let square ?round dims =
  let dims =
    match round with
    | None -> V2.(dims /$ 2.)
    | Some r -> V2.(v ((x dims /. 2.) -. r) ((y dims /. 2.) -. r))
  in
  let f p =
    let dx = V2.(Float.abs (x p) -. x dims)
    and dy = V2.(Float.abs (y p) -. y dims) in
    Float.(neg @@ (V2.norm (v2 (max dx 0.) (max dy 0.)) +. min (max dx dy) 0.))
  in
  match round with
  | None -> f
  | Some r -> fun p -> f p +. r

let rounded_box ?(tl = 0.) ?(tr = 0.) ?(bl = 0.) ?(br = 0.) dims =
  let dx = V2.x dims /. 2.
  and dy = V2.y dims /. 2. in
  fun p ->
    let r =
      match V2.(x p > 0., y p > 0.) with
      | true, true -> tr
      | true, false -> br
      | false, true -> tl
      | false, false -> bl
    in
    let q = V2.(v (Float.abs (x p) -. dx +. r) (Float.abs (y p) -. dy +. r)) in
    Float.(
      neg @@ V2.(min (max (x q) (y q)) 0. +. norm (v (max (x q) 0.) (max (y q) 0.)) -. r))

let rhombus ?round dims =
  let dims =
    match round with
    | None -> V2.(dims /$ 2.)
    | Some r -> V2.(v ((x dims /. 2.) -. r) ((y dims /. 2.) -. r))
  and ndot a b = V2.((x a *. x b) -. (y a *. y b)) in
  let f p =
    let p = V2.(v (Float.abs (x p)) (Float.abs (y p))) in
    let h =
      Math.clamp ~min:(-1.) ~max:1. V2.(ndot (dims -@ (p *$ 2.)) dims /. dot dims dims)
    in
    let d =
      V2.(norm (p -@ v2 ((1. -. h) *. x dims *. 0.5) ((1. +. h) *. y dims *. 0.5)))
    in
    Float.neg
    @@ V2.(d *. Math.sign ((x p *. y dims) +. (y p *. x dims) -. (x dims *. y dims)))
  in
  match round with
  | None -> f
  | Some r -> fun p -> f p +. r

let round r (t : t) p = t p +. r
let onion r (t : t) p = r -. Float.abs (t p)

let elongate h t =
  let hx = V2.x h /. 2.
  and hy = V2.y h /. 2. in
  fun p ->
    let x = Float.abs (V2.x p) -. hx
    and y = Float.abs (V2.y p) -. hy in
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
  let wx = Float.neg @@ t (v2 (V3.x p) (V3.y p))
  and wy = Float.abs (V3.z p) -. (height /. 2.) in
  Float.neg @@ (min (max wx wy) 0. +. V2.norm (v2 (max wx 0.) (max wy 0.)))

let revolve ?offset t =
  match offset with
  | None -> fun p -> t V3.(v2 (V2.norm (v2 (x p) (z p))) (y p))
  | Some o -> fun p -> t V3.(v2 (V2.norm (v2 (x p) (z p)) -. o) (y p))
