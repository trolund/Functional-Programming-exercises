let rec g1 p =
    function
    | x :: xs when p x -> x :: g1 p xs
    | _ -> []


let rec g2 f h n x =
    match n with
    | _ when n < 0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n - 1) (f x)


let g1tail p l =
    let rec aux acc =
        function
        | x :: xs when p x -> aux (x :: acc) xs
        | _ -> List.rev acc

    aux [] l

#I "/Users/troelslund/.nuget/packages/fscheck/3.0.0-alpha4/lib/net452"
#r "FsCheck.dll"
open FsCheck

let pFun x = x > 0
let testList = [ 1; 2; -2; -5; 1; -3; 1; -8 ]

g1 pFun testList
g1tail pFun testList


let test exp = g1 pFun exp = g1tail pFun exp

Check.Quick test

let g1con p l =
    let rec loop cont =
        function
        | x :: xs when p x -> loop (fun acc -> cont (x :: acc)) xs
        | _ -> cont []

    loop id l

g1con pFun testList

let testcon exp = g1 pFun exp = g1con pFun exp

Check.Quick testcon


let f1 m n k =
    seq {
        for x in [ 0 .. m ] do
            for y in [ 0 .. n ] do
                if x + y < k then yield (x, y)
    }

List.ofSeq (f1 2 2 3)
