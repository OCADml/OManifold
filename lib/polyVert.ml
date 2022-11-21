open OCADml
open C.Types

type t =
  { pos : v2
  ; idx : int
  }

let of_struct s =
  let pos = Conv.vec2_to_v2 @@ Ctypes.getf s PolyVert.pos
  and idx = Ctypes.getf s PolyVert.idx in
  { pos; idx }

let to_struct t =
  let s = Ctypes.make PolyVert.t in
  Ctypes.setf s PolyVert.pos (Conv.vec2_of_v2 t.pos);
  Ctypes.setf s PolyVert.idx t.idx;
  s
