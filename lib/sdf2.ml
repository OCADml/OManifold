open OCADml

let circle r p = r -. V2.norm p

let square dims p =
  let dx = Float.abs p.V2.x -. dims.V2.x
  and dy = Float.abs p.y -. dims.y in
  Float.neg @@ (V2.norm Float.(v2 (max dx 0.) (max dy 0.)) +. Float.(min (max dx dy) 0.))

let ndot a b = (a.V2.x *. b.V2.x) -. (a.y *. b.y)

let rhombus dims p =
  let p = v2 (Float.abs p.x) (Float.abs p.y) in
  let h =
    Math.clamp ~min:(-1.) ~max:1. V2.(ndot (dims -@ (p *$ 2.)) dims /. dot dims dims)
  in
  let d = V2.(norm (p -@ v2 ((1. -. h) *. dims.x *. 0.5) ((1. +. h) *. dims.y *. 0.5))) in
  Float.neg @@ (d *. Math.sign ((p.x *. dims.y) +. (p.y *. dims.x) -. (dims.x *. dims.y)))
