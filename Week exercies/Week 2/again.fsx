let f =
    function
    | x when x % 3 = 0 -> true
    | x when x % 2 = 0 -> true
    | _ -> false

f 2
f 10
f 7

// 2.2

let f2 (s, n) =

    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (s + acc) (x - 1)

    aux "" n

f2 ("hej", 3)


// alternativ

let rec f3 (s, n) =
    match n with
    | 0 -> ""
    | n -> s + f3 (s, n - 1)

f3 ("hej", 3)

// 2.13

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y


// 4.3

let evenN n = [ 0 .. 2 .. n ]

evenN 10
