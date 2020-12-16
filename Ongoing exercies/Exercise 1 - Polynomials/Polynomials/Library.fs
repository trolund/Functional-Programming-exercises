namespace Polynomials

open Microsoft.FSharp.Core

module Polynomials =

    (* types *)
    (* types part 1 *)

    type Poly = P of int list

    (* types part 4 *)

    type Degree =
        | MinusInf
        | Fin of int

    (*
        Part 6: The library Polynomial
    *)

    let ofList l = P l

    let toList =
        function
        | P (l) -> l

    // Part 1: Recursive list functions

    let rec simpleOpsAux a b op =
        match a, b with
        | P ([]), P ([]) -> []
        | P ([]), P (b') -> b'
        | P (a'), P ([]) -> a'
        | P (a :: atail), P (b :: btail) -> op a b :: (simpleOpsAux (P(atail)) (P(btail)) op)

    let add a b = P(simpleOpsAux a b ((+)))


    let rec mulC c xs =
        match c, xs with
        | c, _ when c = 0 -> xs
        | _, P ([]) -> P([])
        | c, P (x :: tail) -> P((x * c) :: toList (mulC c (P(tail))))

    let sub a b = P(simpleOpsAux a b ((-)))

    let rec mulX a =
        match a with
        | P ([]) -> P([])
        | P (a) -> P(0 :: a)

    let rec mul x y =
        match x, y with
        | [], [] -> []
        | a, [] -> a
        | [], b -> b
        | a :: atail, q -> add (mulC a y) (mulX (mul atail y))

    let eval x poly =
        let pow x exp = int (float (x) ** float (exp))

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
        let list = toList p

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

        aux (prune list) 0

    let print p = printf "%s" (toString p)

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


    let compose a b =

        let rec ppow p n =
            match (p, n) with
            | (_, -1) -> p
            | (_, 0) -> p
            | (p, n) -> mul p (p_pow p (n - 1))

        let rec composerec a b n =
            match a with
            | head :: tail when n = 0 -> head :: (compose_rec tail b (n + 1)).[1..]
            | head :: tail -> add (mulC head (ppow b (n - 1))) (compose_rec tail b (n + 1))
            | [ last ] -> ppow (mulC last b) n
            | [] -> []

        composerec a b 0

    let deg =
        function
        | [] -> MinusInf
        | x :: tail when x <= 0 && tail.IsEmpty -> MinusInf
        | _ :: tail when tail.IsEmpty -> (Fin 0)
        | x :: tail ->
            Fin
                ((List.fold (fun x acc -> acc + 1) 0 (x :: tail))
                 + 1)


    let addD d1 d2 =
        match (d1, d2) with
        | _, MinusInf -> MinusInf
        | MinusInf, _ -> MinusInf
        | Fin (x), Fin (y) -> Fin(x + y)

    (*
        compose TODO https://www.youtube.com/watch?v=OOnKGqNyv50
    *)


    (* TODO Prefix Operators overload *)

    type Poly with
        static member (+)(a: Poly, b: Poly) = add a b
        static member (*)(a: Poly, b: Poly) = mul a b
        static member (*)(a: int, b: Poly) = mulC a b
        static member (-)(a: Poly, b: Poly) = mul a b
        member x.Print() = print x
        member x.ToString() = toString x
