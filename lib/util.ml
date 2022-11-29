let fa = Float.pi /. 18.

(* Based on:
https://github.com/openscad/openscad/blob/5e1a7cddd26de6fcfee753ee1d0fde5c90f785cd/src/calc.cc#L72 *)
let helical_slices ?fn ?(fa = fa) twist =
  let twist = Float.abs twist in
  let min_slices = Int.max 1 Float.(to_int @@ ceil @@ (twist /. (pi /. 3.))) in
  ( match fn with
  | Some n -> Float.(to_int @@ ceil @@ (of_int n *. twist /. (2. *. pi)))
  | None -> Float.(to_int @@ ceil (twist /. fa)) )
  |> Int.max min_slices
