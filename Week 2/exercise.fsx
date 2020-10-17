// HR 2.1, 2.2, 2.13, 4.3, 4.8, 4.9, 4.12

// Exercise 2.1
let f21 x =
    match x with
    | _ when x % 5 = 0 -> false
    | _ when x % 2 = 0 -> true
    | _ when x % 3 = 0 -> true
    | _ -> false

let f21v2 x =
    match x with
    | _ when x % 2 = 0 && x % 5 <> 0 -> true
    | _ when x % 3 = 0 && x % 5 <> 0 -> true
    | _ -> false


f21 24
f21 27
f21 29
f21 30


// Exercise 2.2
let rec f22 (s, n) =
    match n with
    | 0 -> ""
    | _ when n > 0 -> s + f22 (s, n - 1)
    | _ -> s

f22 ("Hej", 2)



// Exercise 2.13
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

curry f22

// Exercise 4.3
let rec evenN n =
    match n with
    | a when a = 0 -> []
    | a when (a % 2) = 1 -> evenN (n - 1)
    | a -> a :: evenN (a - 2)

let evenN2 n = [ 0 .. 2 .. n ]

evenN 12
evenN 13
evenN2 12
evenN2 13

// Exercise 4.8
let rec split l =
    match l with
    | [] -> ([], [])
    // | [x] -> (x,[])
    | x :: y :: tail ->
        let (e, u) = split tail
        (x :: e, y :: u)


split [ 1; 2; 3; 4; 5; 6; 7; 8 ]


// Exercise 4.9
let rec zip (l1: int list, l2: int list) =
    match (l1, l2) with
    | [], [] -> []
    | [], x :: tail -> (0, x) :: zip ([], tail)
    | x :: tail, [] -> (x, 0) :: zip (tail, [])
    | x1 :: tail1, x2 :: tail2 -> (x1, x2) :: zip (tail1, tail2)

zip ([ 1; 2; 3; 4; 5 ], [ 1; 2; 3; 4; 5 ])
zip ([ 1; 2; 3; 4; 5 ], [ 1; 2; 3; 4; 5; 6 ])
zip ([ 1; 2; 3; 4; 5; 6 ], [ 1; 2; 3; 4; 5 ])


// Exercise 4.12
let rec sum p xs =
    match xs with
    | [] -> 0
    | [ x ] -> if p x then x else 0
    | x :: tail -> if p x then x + (sum p tail) else sum p tail

let pred x = x > 2 && x < 10
// let pred x = x = x;;

sum pred [ 1; 2; 3; 4; 11 ]
