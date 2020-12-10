#I "/Users/troelslund/.nuget/packages/fscheck/3.0.0-alpha4/lib/net452"
#r "FsCheck.dll"
open FsCheck

(* Problem 2 (25%) *)

// 1

let multTable n =
    Seq.take 10 (Seq.initInfinite (fun i -> n * (i + 1)))

multTable 3

// 2 todo Fejl!!

let tableOf m n f =
    seq {
        for i in 1 .. m do
            for j in 1 .. n do
                yield (i, j, f i j)
    }

tableOf 3 4 (+)


// 3

let is =
    let rec s acc n =
        match n with
        | 0 -> acc
        | _ -> s (acc + "a") (n - 1)

    Seq.initInfinite (fun i -> s "" (i + 1))

is


// 4

//The function f computes a list of numbers. The numbers are the sum of x and i, where i is squared every time f is called.

// 5

let rec f i =
    function
    | [] -> []
    | x :: xs -> (x + i) :: f (i * i) xs


let f2 i xs =
    let rec aux acc i =
        function
        | [] -> List.rev acc
        | x :: xs -> aux ((x + i) :: acc) (i * i) xs

    aux [] i xs


let f3 i xs =
    let rec loop cont i =
        function
        | x :: xs -> loop (fun acc -> cont ((x + i) :: acc)) (i * i) xs
        | _ -> cont []

    loop id i xs

let test exp = f 1 exp = f2 1 exp && f 1 exp = f3 1 exp

Check.Quick test
