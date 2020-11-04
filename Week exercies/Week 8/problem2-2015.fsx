(* Problem 2 from the exam fall 2015 *)

let rec g1 p = 
    function
    | x::xs when p x -> x :: g1 p xs
    | _ -> []

let rec g2 f h n x =
    match n with
    | _ when n<0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n-1) (f x);;

(* 
    2. The function g1 is not tail recursive.
    • Make a tail-recursive variant of g1 using an accumulating parameter.
    • Make a continuation-based tail-recursive variant of g1.
*)

g1 (fun x -> true) [1;2;3;4;5]

let g1tail (p, l: int list) = 
    let rec aux acc = function
            | x::xs when p x -> aux (x :: acc) xs
            | _ -> List.rev acc

    aux [] l

g1tail ((fun x -> x = x), [1;2;3;4;5])