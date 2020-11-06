(*
    9.3 Declare an iterative solution to exercise 1.6.

    Exercise 1.6:

        1.6) Declare a recursive function sum: int * int -> int, where
        sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
        for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
        Give the recursion formula corresponding to the declaration.

        My solution to 1.6

        let rec sum =
            function
            | (m, 0) -> m
            | (m, n) -> m + n + sum (m, n - 1)

        sum (4, 5)
*)

let sum (m, n) =
    let rec aux =
        function
        | (m, 0, acc) -> m + acc
        | (m, n, acc) -> aux (m, n - 1, m + n + acc)

    aux (m, n, 0)


sum (4, 5) // 39

(* Old solution *)

let rec oldsum =
    function
    | (m, 0) -> m
    | (m, n) -> m + n + oldsum (m, n - 1)

oldsum (4, 5) // 39


(*
    9.4 Give iterative declarations of the list function List.length.
*)

let length l =

    let rec lenAux acc =
        function
        | [] -> acc
        | _ :: tail -> lenAux (1 + acc) tail

    lenAux 0 l

length [ 1; 2; 3; 4; 5; 6 ]


let length2 l = List.fold (fun acc _ -> acc + 1) 0 l

length2 [ 1; 2; 3; 4; 5; 6 ]


