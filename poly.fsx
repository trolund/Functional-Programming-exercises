type Poly = int list


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

let reduce f list =
    match list with
    | head :: tail -> List.fold f head tail
    | [] -> failwith "The list was empty!"

// virker eval???

let rec eval x p =
    match x, p with
    | _, [] -> 1
    | x, p :: tail -> (pown x p) + eval x tail

// eval 2 [ 2; 3; 0; 1 ] // 16

// eval 2 [ 0; 1; 0; 1 ] // 11
