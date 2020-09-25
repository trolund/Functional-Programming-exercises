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

// let rec evalGen x p i =
//     match p with
//     | [] -> 0
//     | exp :: tail -> (exp * pow x i + evalGen x tail (i + 1))

// let eval x poly = evalGen x poly 1


let eval x poly =

    let rec pow x exp = int (float (x) ** float (exp))

    let rec evalGen x p i =
        match p with
        | [] -> 0
        | exp :: tail -> (exp * pow x i + evalGen x tail (i + 1))

    evalGen x poly 1

eval 2 [ 2; 3; 0; 1 ]

let addThreeNumbers x y z =

    //create a nested helper function
    let add n = fun x -> x + n

    // use the helper function
    x |> add y |> add z

pow 5 2

let rec fact x = if x < 1 then 1 else x * fact (x - 1)


// eval 2 [ 2; 3; 0; 1 ] // 16
// eval 2 [ 0; 1; 0; 1 ] // 11



// Part 2: functional decomposition

let rec toString p: int list =
    match p with
    | [] -> []
    | p :: tail -> (sprintf "%i" + p) + toString tail
