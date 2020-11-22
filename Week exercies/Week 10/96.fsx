(* 

    Declare a continuation-based version of the factorial function and compare the run time with
    the results in Section 9.4.

    9.4 Give iterative declarations of the list function List.length.

*)

let lengthContinuation l =
    let rec lenAux acc =
        match acc with
        | [] -> 0
        | _ :: tail -> let x = lenAux tail
                       1 + x

    lenAux l


lengthContinuation [ 1; 2; 3; 4; 5; 6 ]


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