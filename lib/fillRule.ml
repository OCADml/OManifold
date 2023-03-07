type t = C.Types.FillRule.t =
  | EvenOdd
  | NonZero
  | Positive
  | Negative

let make = function
  | `EvenOdd -> EvenOdd
  | `NonZero -> NonZero
  | `Positive -> Positive
  | `Negative -> Negative
