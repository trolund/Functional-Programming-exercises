let rec mixMap f x y =
    match x, y with
    | [], [] -> []
    | x, [] -> x
    | [], y -> y
    | x :: xTail, y :: yTail -> (f x y) :: (mixMap f xTail yTail)

mixMap
    ((+))
    [ 1; 2; 3; 4 ]
    [ 5
      6
      7
      8 ] // (( + )) er det samme som (fun x y -> x + y)


let rec unmixMap f g v =

    let rec xf =
        function
        | [] -> []
        | (x, _) :: tail -> (f x) :: (xf tail)

    let rec yf =
        function
        | [] -> []
        | (_, y) :: tail -> (g y) :: (yf tail)

    (xf v, yf v)

unmixMap id id [ (1, 2); (3, 4); (5, 6) ]
