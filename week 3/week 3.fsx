let rec f g =
    function
    | [] -> []
    | x :: xs -> g x :: f (fun y -> g (g y)) xs
