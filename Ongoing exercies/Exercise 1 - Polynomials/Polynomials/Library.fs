namespace Polynomials

module Polynomials = 

    (* types *)
    (* types part 1 *)

    type Poly = int list

    (* types part 4 *)

    // type MinusInf = int
    // type Fin = int
    type Degree = 
        | MinusInf
        | Fin of int    
    (*
    let p1 = ofList [1; 2];;
    // val p1 : Polynomial.Poly = 1 + 2x
    let p2 = ofList [3;4;5];;
    // val p2 : Poly = 3 + 4x + 5x^2
    let p3 = ofList [0;0;0;0;2];;
    // val p3 : Poly = 2x^4
    let p4 = p1 + p2*p3;;
    // val p4 : Poly = 1 + 2x + 6x^4 + 8x^5 + 10x^6
    let p5 = compose p4 p3;;
    // val p5 : Poly = 1 + 4x^4 + 96x^16 + 256x^20 + 640x^24
    let p6 = derivative p5;;
    // val p6 : Poly = 16x^3 + 1536x^15 + 5120x^19 + 15360x^23
    let d = max (deg p4) (deg p6);;
    // val d : Degree = Fin 23 
    *)

    // Part 1: Recursive list functions

    let rec simpleOpsAux a b op = 
         match a, b with
            | [], [] -> []
            | [], b -> b
            | a, [] -> a
            | a :: atail, b :: btail -> op a b :: (simpleOpsAux atail btail op)

    let add a b = simpleOpsAux a b (( + ))
    

    let rec mulC c xs =
        match c, xs with
        | c, _ when c = 0 -> xs
        | _, [] -> []
        | c, x :: tail -> (x * c) :: (mulC c tail)

  

    let sub a b = simpleOpsAux a b (( - ))

 

    let rec mulX a =
        match a with
        | [] -> []
        | a -> 0 :: a


    // TODO Something is wrong!
    let rec mul x y =
        match x, y with
        | [], [] -> []
        | a, [] -> a
        | [], b -> b
        | a :: atail, q -> add (mulC a q) (mulX (mul atail q))

    let eval x poly =
        let rec pow x exp = int (float (x) ** float (exp))

        let rec cal x p i =
            match p with
            | [] -> 0
            | exp :: tail -> (exp * pow x i + cal x tail (i + 1))

        cal x poly 1

    // Part 2: Functional decomposition

    let rec isLegal poly =
        match poly with
        | [] -> true
        | [ x ] when x = 0 -> false // | [ 0 ] -> [] ?? could simplify it.
        | [ _ ] -> true
        | _ :: tail -> isLegal tail

    let sum poly =
        let rec reduceSum (acc: int, poly: int list) =
            match acc, poly with
            | _, [] -> 0
            | _, head :: tail -> acc + head + reduceSum (acc, tail)

        reduceSum (0, poly)

    let rec prune poly =
        match poly with
        | [] -> []
        | [ 0 ] -> []
        | head :: tail when sum tail = 0 -> [ head ]
        | head :: tail -> head :: prune tail


    let toString p =
        let rec aux p i =
            match (p, i) with
            | [], _ -> ""
            | head :: tail, i when i = 0 -> string head + aux tail (i + 1)
            | head :: tail, i when i = 1 ->
                match head with
                | x when x = 0 -> aux tail (i + 1)
                | x when x = 1 -> "+" + "x" + aux tail (i + 1)
                | x when x = -1 -> "-" + "x" + aux tail (i + 1)
                | x when x > 0 -> "+" + string head + "x" + aux tail (i + 1)
                | _ -> string head + "x" + aux tail (i + 1) // minus
            | head :: tail, _ ->
                match head with
                | x when x = 0 -> aux tail (i + 1)
                | x when x = 1 -> "+" + "x^" + string i + aux tail (i + 1)
                | x when x = -1 -> "-" + "x^" + string i + aux tail (i + 1)
                | x when x > 0 ->
                    "+"
                    + string head
                    + "x^"
                    + string i
                    + aux tail (i + 1)
                | _ -> string head + "x^" + string i + aux tail (i + 1) // minus

        aux (prune p) 0


    let rec derivative poly =
        let rec aux p i =
            match p, i with
            | [], _ -> []
            | _ :: tail, i when i = 0 -> 0 :: aux tail (i + 1) // k = 0
            | head :: tail, i when i = 1 ->
                match head with
                | x when x = 1 -> 1 :: aux tail (i + 1)
                | x -> aux tail (i + 1)
            // head :: aux tail (i + 1) // x = 1
            | head :: tail, i -> i * head :: aux tail (i + 1)

        aux (prune poly) 0

(*
    TODO
    The function compose: Poly -> Poly -> Poly
*)

    // let rec compose (p1:Poly) (p2: Poly) = 
    //     match p1, p2 with 
    //     | [], y::ytail -> y
    //     | x::xtail, y::ytail -> x * y + compose xtail ytail

    let deg = function
        | [] -> MinusInf
        | x::tail when x <= 0 && tail.IsEmpty -> MinusInf
        | _::tail when tail.IsEmpty -> (Fin 0)
        | x::tail -> Fin((List.fold (fun x acc -> acc + 1 ) 0 (x::tail)) + 1)


    let addD d1 d2 =    
        match (d1, d2) with 
        | _, MinusInf -> MinusInf
        | MinusInf, _  -> MinusInf
        | Fin(x), Fin(y) -> Fin(x + y)

    