// Exercises week 1

open System

// #1 (1.4)
let rec f (n: int) = if n >= 0 then n + f (n - 1) else n + 1

printfn "Result %d" (f 4)

// f(4) evaluates to 10 = 1+2+3+4

// #2 (1.6)
//let rec sum (m: int, n: int) = if n >= 0 then n + f (n - 1) else n + 1

let rec sum =
    function
    | (m, 0) -> m
    | (m, n) -> m + n + sum (m, n - 1)

sum (4, 5)


// #3 (2.8)

let rec bin n k =
    match (n, k) with
    | (_, 0) -> 1
    | (n, k) when n = k -> 1
    | (n, k) -> bin (n - 1) (k - 1) + bin (n - 1) k

bin 4 2

// #4 (4,7)

let rec multiplicity x xs =
    match xs with
    | [] -> 0
    | y :: tail when x = y -> 1 + multiplicity x tail
    | y :: tail -> multiplicity x tail

multiplicity 1 [ 2; 1; 1; 2 ]
