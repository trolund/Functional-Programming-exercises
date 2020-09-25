type Poly = int list

// Part 1: Recursive list functions

let rec add a b =
    match a, b with
    | [], [] -> []
    | [], b -> b
    | a, [] -> a
    | a :: atail, b :: btail -> (a + b) :: (add atail btail)

add [ 1; 2 ] [ 3; 4; 5; 6 ]


let rec mulC c xs =
    match c, xs with
    | c, _ when c = 0 -> xs
    | _, [] -> []
    | c, x :: tail -> (x * c) :: (mulC c tail)

mulC 2 [ 2; 0; 0; 1 ]

let rec sub a b =
    match a, b with
    | [], [] -> []
    | [], b -> b
    | a, [] -> a
    | a :: atail, b :: btail -> (a - b) :: (sub atail btail)

sub [ 1; 2 ] [ 3; 4; 5; 6 ]

let rec mulX a =
    match a with
    | [] -> []
    | a -> 0 :: a

mulX [ 2; 0; 0; 1 ]

// Something is wrong!
let rec mul x y =
    match x, y with
    | [], [] -> []
    | a, [] -> a
    | [], b -> b
    | a :: atail, q -> add (mulC a q) (mulX (mul atail q))

// mul [ 2; 0; 0; 1 ] [ 2; 0; 0; 1 ]
mul [ 2; 3; 0; 1 ] [ 1; 2; 3 ]

let eval x poly =

    let rec pow x exp = int (float (x) ** float (exp))

    let rec cal x p i =
        match p with
        | [] -> 0
        | exp :: tail -> (exp * pow x i + cal x tail (i + 1))

    cal x poly 1

eval 2 [ 2; 3; 0; 1 ]

// Part 2: functional decomposition
// let rec toString p: int list =
//     match p with
//     | [] -> []
//     | p :: tail -> (sprintf "%i" + p) + toString tail

let rec isLegal poly =
    match poly with
    | [] -> true
    | [ x ] when x = 0 -> false
    | [ x ] -> true
    | head :: tail -> isLegal tail

isLegal [ 2; 3; 0; 1; 0 ]
isLegal [ 2; 3; 0; 1 ]
