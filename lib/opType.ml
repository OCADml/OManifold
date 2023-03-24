type t = C.Types.OpType.t =
  | Add
  | Subtract
  | Intersect

let make = function
  | `Add -> Add
  | `Subtract -> Subtract
  | `Intersect -> Intersect
