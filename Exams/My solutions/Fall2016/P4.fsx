#I "/Users/troelslund/.nuget/packages/fscheck/3.0.0-alpha4/lib/net452"
#r "FsCheck.dll"
open FsCheck

type Tree<'a, 'b> =
    | A of 'a
    | B of 'b
    | Node of Tree<'a, 'b> * Tree<'a, 'b>

// 1.

let exp = Node(A "a", B "b")
let exp2 = A "a"
let exp3 = Node(A "a", Node(A "a", B "b"))

// 2.

let rec countA =
    function
    | Node (t1, t2) -> countA t1 + countA t2
    | B (_) -> 0
    | A (_) -> 1

countA exp3

// 3.

let rec subst a a' b b' =
    function
    | Node (t1, t2) -> Node(subst a a' b b' t1, subst a a' b b' t2)
    | B (v) -> if v = b then B b' else B v
    | A (v) -> if v = a then A a' else A v


subst "a" "a'" "b" "b'" exp3


// 4.

let rec g =
    function
    | Node (t1, t2) -> Node(g t2, g t1)
    | leaf -> leaf

let rec f =
    function
    | A a -> ([ a ], [])
    | B b -> ([], [ b ])
    | Node (t1, t2) ->
        let (xs1, ys1) = f t1
        let (xs2, ys2) = f t2
        (xs1 @ xs2, ys1 @ ys2)
(*
    g computes:

    g spejler et træ.


    f computes:

    f splitter a og b værdier op i hver sin liste.

*)


// 5.

let fK t =

    let rec aux t k =
        match t with
        | A a -> k ([ a ], [])
        | B b -> k ([], [ b ])
        | Node (t1, t2) -> aux t1 (fun (xs1, ys1) -> aux t2 (fun (xs2, ys2) -> k (xs1 @ xs2, ys1 @ ys2)))

    aux t id

let test exp = f exp = fK exp

Check.Quick test
