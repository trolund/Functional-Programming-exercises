let rec f xs ys =
    match (xs, ys) with
    | (x :: xs1, y :: ys1) -> x :: y :: f xs1 ys1
    | _ -> []

f [ 1; 6; 0; 8 ] [ 0; 7; 3; 3 ]
