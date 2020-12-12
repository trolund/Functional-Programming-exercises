(* Problem 2 (15%) *)


// 1.

let rec replace a b =
    function
    | [] -> []
    | head :: tail when head = a -> b :: replace a b tail
    | head :: tail -> head :: replace a b tail

replace 2 7 [ 1; 2; 3; 2; 4 ] = [ 1; 7; 3; 7; 4 ]

// 2.
// 'a -> 'a -> ('a list -> 'a list)

// 3

let replacetail a b =
    let rec aux acc =
        function
        | [] -> List.rev acc
        | head :: tail when head = a -> aux (b :: acc) tail
        | head :: tail -> aux (head :: acc) tail

    aux []
