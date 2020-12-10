#I "/Users/troelslund/.nuget/packages/fscheck/3.0.0-alpha4/lib/net452"
#r "FsCheck.dll"
open FsCheck


type Rel<'a, 'b> = ('a * 'b list) list

let rel: Rel<int, string> =
    [ (1, [ "a"; "b"; "c" ])
      (4, [ "b"; "e" ]) ]

(* Problem 1 (20%) *)
// 1

let rec apply a =
    function
    | [] -> []
    | (key, rel) :: _ when a = key -> rel
    | _ :: tail -> apply a tail

apply 1 rel
apply 0 rel

// 2

let rec inRelation x y =
    function
    | [] -> false
    | (key, rel) :: _ when List.contains y rel && x = key -> true
    | _ :: tail -> inRelation x y tail

inRelation 4 "e" rel
inRelation 1 "e" rel

// alternativ

let rec inRelation2 x y rel = checkList y (apply x rel)

and checkList y =
    function
    | [] -> false
    | x :: xs -> y = x || checkList y xs


let test exp =
    inRelation 4 "e" exp = inRelation 4 "e" exp

Check.Quick test

// 3

let rec insert x y =
    function
    | [] -> [ x, [ y ] ] // tilføj ny
    | (key, rel) :: tail when x = key -> (key, y :: rel) :: tail // tilføj til eksiterende
    | head :: tail -> head :: (insert x y tail) // loop!

insert 2 "c" [ (1, [ "a" ]); (2, [ "b" ]) ]

insert 1 "a" [ (1, [ "c" ]); (2, [ "b" ]) ]

insert 3 "d" [ (1, [ "c" ]); (2, [ "b" ]) ]


// 4

let toRel l =
    let rec loop acc =
        function
        | [] -> acc
        | (k, v) :: tail -> loop (insert k v acc) tail

    loop [] l

toRel [ (2, "c"); (1, "a"); (2, "b") ]
