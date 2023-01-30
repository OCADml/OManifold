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

let iter2i f l1 l2 =
  let rec loop i l1 l2 =
    match l1, l2 with
    | [], [] -> ()
    | h1 :: t1, h2 :: t2 ->
      f i h1 h2;
      loop (i + 1) t1 t2
    | _ -> invalid_arg "iter2i: lists are of unequal length"
  in
  loop 0 l1 l2
