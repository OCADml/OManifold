type t = C.Types.JoinType.t =
  | Square
  | Round
  | Miter

let make = function
  | `Square -> Square
  | `Round -> Round
  | `Miter -> Miter
