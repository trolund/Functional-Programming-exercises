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

// Part 2: Functional decomposition

let rec isLegal poly =
    match poly with
    | [] -> true
    | [ x ] when x = 0 -> false // | [ 0 ] -> [] ?? could simplify it.
    | [ _ ] -> true
    | _ :: tail -> isLegal tail

isLegal [ 2; 3; 0; 1; 0 ]
isLegal [ 2; 3; 0; 1 ]

let rec sum poly =

    let rec reduceSum (acc: int, poly: int list) =
        match acc, poly with
        | _, [] -> 0
        | _, head :: tail -> acc + head + reduceSum (acc, tail)

    reduceSum (0, poly)

sum [ 2; 3; 0; 1; 0; 0; 0 ]

let rec prune poly =
    match poly with
    | [] -> []
    | [ 0 ] -> []
    | head :: tail when sum tail = 0 -> [ head ]
    | head :: tail -> head :: prune tail

prune [ 2; 3; 0; 1; 0 ]

prune [ 2; 3; 0; 1; 0; 0; 0 ]

let rec toString p =

    let rec reduce p i =
        match (p, i) with
        | [], _ -> ""
        | head :: tail, _ when head = 0 -> reduce tail (i + 1)
        | head :: tail, i when i = 1 ->
            match i with
            | x when x = 0 -> "" + reduce tail (i + 1)
            | x when x > 0 -> string head + "x" + reduce tail (i + 1)
            | x when x < 0 -> "-" + string head + "x" + reduce tail (i + 1)
        | head :: tail, _ when head < 0 ->
            string head
            + "x^"
            + string i
            + reduce tail (i + 1)
        | head :: tail, _ ->
            "+"
            + string head
            + "x^"
            + string i
            + reduce tail (i + 1)

    reduce (prune p) 1

toString [ 2; 3; 0; 1; 0 ]
toString [ 2; -3; 0; 1; 0 ]
