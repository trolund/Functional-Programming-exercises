let rec f n =
    function
    | 0 -> 1
    | k when k > 0 -> n * (f n (k - 1))
    | _ -> failwith "illegal argument"

let rec g p f =
    function
    | [] -> []
    | x :: xs when p x -> f x :: g p f xs
    | _ :: xs -> g p f xs

type T =
    | A of int
    | B of string
    | C of T * T

let rec h =
    function
    | A n -> string n
    | B s -> s
    | C (t1, t2) -> h t1 + h t2

let sq = Seq.initInfinite (fun i -> 3 * i)

let k j =
    seq {
        for i in sq do
            yield (i, i - j)
    }

k 3

let xs = Seq.toList (Seq.take 4 sq)
let ys = Seq.toList (Seq.take 4 (k 2))

f 2 3

g (fun x -> x > 0) (fun y -> y + 1) [ -1; -2; -3; 4; 5 ]

h (C(A 1, B "2"))

#I "/Users/troelslund/.nuget/packages/fscheck/3.0.0-alpha4/lib/net452"
#r "FsCheck.dll"
open FsCheck

let ftail n x =

    let rec loop acc =
        function
        | 0 -> acc
        | k when k > 0 -> loop (acc * n) (k - 1)
        | _ -> failwith "illegal argument"

    loop 1 x

let fcon n x =

    let rec loop f =
        function
        | 0 -> f 1
        | k when k > 0 -> loop (fun temp -> f (temp * n)) (k - 1)
        | _ -> failwith "illegal argument"

    loop id x

fcon 2 3
ftail 2 3
f 2 3

fcon 4 8
ftail 4 8
f 4 8

let test exp exp2 =
    f exp 8 = f exp 8 && f exp 8 = fcon exp 8

Check.Quick test
