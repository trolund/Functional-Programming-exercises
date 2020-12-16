#I "/Users/troelslund/.nuget/packages/fscheck/3.0.0-alpha4/lib/net452"
#r "FsCheck.dll"
open FsCheck

let rec f xs ys =
    match (xs, ys) with
    | (x :: xs1, y :: ys1) -> x :: y :: f xs1 ys1
    | _ -> []

f [ 1; 6; 0; 8 ] [ 0; 7; 3; 3 ]

let rec f2 xs ys =
    let rec loop acc =
        function
        | x :: xs1, y :: ys1 -> loop (x :: y :: acc) (xs1, ys1)
        | _ -> List.rev acc

    loop [] (xs, ys)

f2 [ 1; 6; 0; 8 ] [ 0; 7; 3; 3 ]

let rec f3 xs ys =
    let rec loop cont =
        function
        | (x :: xs1, y :: ys1) -> loop (fun acc -> cont (x :: y :: acc)) (xs1, ys1)
        | _ -> cont []

    loop id (xs, ys)


let test exp =
    f exp exp = f2 exp exp && f exp exp = f3 exp exp

Check.Quick test
