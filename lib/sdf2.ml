open OCADml

type t = v2 -> float

let circle r p = r -. V2.norm p

let square dims p =
  let dx = Float.abs p.V2.x -. dims.V2.x
  and dy = Float.abs p.y -. dims.y in
  Float.neg @@ (V2.norm Float.(v2 (max dx 0.) (max dy 0.)) +. Float.(min (max dx dy) 0.))

let rhombus dims p =
  let ndot a b = (a.V2.x *. b.V2.x) -. (a.y *. b.y) in
  let p = v2 (Float.abs p.x) (Float.abs p.y) in
  let h =
    Math.clamp ~min:(-1.) ~max:1. V2.(ndot (dims -@ (p *$ 2.)) dims /. dot dims dims)
  in
  let d = V2.(norm (p -@ v2 ((1. -. h) *. dims.x *. 0.5) ((1. +. h) *. dims.y *. 0.5))) in
  Float.neg @@ (d *. Math.sign ((p.x *. dims.y) +. (p.y *. dims.x) -. (dims.x *. dims.y)))

let round r (t : t) p = t p +. r
let onion r (t : t) p = r -. Float.abs (t p)

let extrude ~height t p =
  let wx = Float.neg @@ t (v2 p.V3.x p.y)
  and wy = Float.abs p.z -. (height /. 2.) in
  Float.neg @@ (min (max wx wy) 0. +. V2.norm (v2 (max wx 0.) (max wy 0.)))

let revolve ~radius t p = t @@ v2 (V2.norm (v2 p.V3.x p.z) -. radius) p.y
