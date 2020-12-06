let rec ordered ol =
    let rec aux predecessor =
        function
        | [] -> true
        | head :: tail when head >= predecessor -> aux head tail && true
        | head :: tail -> aux head tail && false

    aux -999999 ol

ordered [ 1; 2; 3; 4; 7; 8; 99 ]
ordered [ 1; 2; 2; 8; 7; 100; 99 ]


let smallerThanAll x xs = List.forall (fun i -> x < i) xs


smallerThanAll
    1
    [ 2
      3
      4
      7
      8
      99 ] // TRUE

smallerThanAll
    3
    [ 2
      3
      4
      7
      8
      99 ] // FALSE


let rec insertBefore p x =
    function
    | [] -> []
    | head :: tail -> if p head then x :: head :: tail else head :: insertBefore p x tail

insertBefore (fun x -> x > 2) 3 [ 0; 1; 2; 4; 5 ]


type Sex =
    | M // male
    | F // female

let sexToString =
    function
    | M -> "Male"
    | F -> "Female"

sexToString F


let replicate n str =

    let rec loop acc n =
        match n with
        | n when n < 0 -> failwith "n can't be negative!"
        | 0 -> acc
        | n -> loop (str + acc) (n - 1)

    loop "" n

replicate 3 "hej"
replicate -3 "hej"
