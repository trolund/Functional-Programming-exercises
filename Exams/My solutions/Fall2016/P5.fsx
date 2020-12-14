type T<'a> = N of 'a * T<'a> list


let td = N("g", [])
let tc = N("c", [ N("d", []); N("e", [ td ]) ])
let tb = N("b", [ N("c", []) ])
let ta = N("a", [ tb; tc; N("f", []) ])


// 1.

let rec tolist =
    function
    | N (n, sub) -> n :: aux sub

and aux =
    function
    | [] -> []
    | x :: xs -> tolist x @ aux xs

tolist ta

// 2.

let map f t =
    let rec mapList =
        function
        | [] -> []
        | n :: ns -> (nodeMap n) :: (mapList ns)

    and nodeMap =
        function
        | N (name, children) -> N(f name, mapList children)

    nodeMap t


map (fun x -> x) ta
map (fun x -> x + " hej") ta
map (fun x -> if x = "c" then "seeee" else x) ta


// alternativ løsning:

(*
    Løsning hvor funktionen f bliver parset rundt som parameter.
*)

let rec mapL f =
    function
    | [] -> []
    | n :: ns -> (map2 f n) :: (mapL f ns)

and map2 f =
    function
    | N (name, children) -> N(f name, mapL f children)


// 3.

let rec isPath is t =
    match is, t with
    | [], _ -> true
    | p :: px, N (name, children) ->
        match List.tryItem p children with
        | None -> false
        | Some (v) -> isPath px v

isPath [ 1; 1; 0 ] ta

// 4.

let rec get is t =
    match is, t with
    | [], _ -> t
    | p :: px, N (name, children) -> get px (List.item p children)

get [ 1; 1; 0 ] ta

// 5.


let rec tryFindPathTo v =
    function
    | N (v', ts) -> if v = v' then Some [] else tryFindInList 0 v ts

and tryFindInList i v =
    function
    | [] -> None
    | N (v', _) :: ts when v = v' -> Some [ i ]
    | N (_, ts') :: ts ->
        match tryFindInList 0 v ts' with
        | None -> tryFindInList (i + 1) v ts
        | Some is -> Some(i :: is)


tryFindPathTo "g" ta
