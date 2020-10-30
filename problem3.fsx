(* Problem 3 from the exam fall 2015 *)

let rec g1 p = 
    function
    | x::xs when p x -> x :: g1 p xs
    | _ -> []

let rec g2 f h n x =
    match n with
    | _ when n<0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n-1) (f x);;