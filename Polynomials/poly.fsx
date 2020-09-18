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

let rec mul a b =
    match a, b with
    | [], [] -> []
    | [], b -> b
    | a, [] -> a
    | a :: atail, b :: btail -> (a * b + mul atail btail) :: (mul atail btail)

mul [ 2; 0; 0; 1 ] [ 2; 0; 0; 1 ]

// virker eval???
// let rec eval x p =
//     match p with
//     | [] -> 0
//     | p :: tail ->
//         (pown x (tail.GetReverseIndex () -1))
//         + eval x tail
// eval 2 [ 2; 3; 0; 1 ] // 16
// eval 2 [ 0; 1; 0; 1 ] // 11



// Part 2: functional decomposition

let rec toString p: int list =
    match p with
    | [] -> []
    | p :: tail -> (sprintf "%i" + p) + toString tail
