type Tree<'a, 'b> =
    | A of 'a
    | B of 'b
    | Node of Tree<'a, 'b> * Tree<'a, 'b>


let valueOne = Node(A "a", B "b")
let valueTwo = Node(Node(A "a", B "b"), A "a")

let valuethree =
    Node(Node(A "a", Node(A "a", B "b")), A "a")



let countA tree =

    let rec loop tree count =
        match tree with
        | Node (x, y) -> loop x 0 + loop y 0
        | A (_) -> 1
        | B (_) -> 0

    loop tree 0

countA valueTwo


let rec subst a am b bm =
    function
    | A (x) -> am
    | B (y) -> bm
    | Node (x, y) -> Node(subst x, subst y)
    | n -> n



type T<'a> = N of 'a * T<'a> list

let td = N("g", [])
let tc = N("c", [ N("d", []); N("e", [ td ]) ])
let tb = N("b", [ N("c", []) ])
let ta = N("a", [ tb; tc; N("f", []) ])


let rec namesList =
    function
    | [] -> []
    | n :: ns -> (namesNode n) @ (namesList ns)

and namesNode =
    function
    | N (name, children) -> name :: (namesList children)

let toList node = namesNode node

toList ta

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



let rec mapL f =
    function
    | [] -> []
    | n :: ns -> (map f n) :: (mapL f ns)

and map f =
    function
    | N (name, children) -> N(f name, mapL f children)

map (fun x -> x) ta
map (fun x -> x + " hej") ta
map (fun x -> if x = "c" then "seeee" else x) ta


type Path = int list

let rec path t =
    function
    | [] -> false
    | n :: ns -> (isPath t n) || (path t ns)

and isPath is t =
    match is with
    | node when node = t -> true
    | N (_, children) -> false || (path t children)

isPath ta tb
isPath ta td

let tc = N("c", [ N("d", []); N("e", [ td ]) ])


let path =
    [ 1
      1
      0 ] // path to ta in td

// iterate over to lists
let rec getChildren =
    function
    | [], children -> children
    | head :: tail, children -> [ get tail (List.item head children) ]

and get (path: Path) =
    function
    | N (name, children) -> N(name, getChildren path children)


get path td
