(* Problem 4 (35%) *)


type Outcome =
    | S
    | F // S: for success and F: for failure

type Sample = Outcome list

type ProbTree =
    | Branch of string * float * ProbTree * ProbTree
    | Leaf of string


let exp =
    Branch(">2", 0.67, Branch(">3", 0.5, Leaf "A", Leaf "B"), Branch(">3", 0.5, Leaf "C", Leaf "D"))

let exp2 =
    Branch(">2", -0.67, Branch(">3", 0.5, Leaf "A", Leaf "B"), Branch(">3", 0.5, Leaf "C", Leaf "D"))

let exp3 =
    Branch(">2", 0.67, Branch(">3", 1.5, Leaf "A", Leaf "B"), Branch(">3", 0.5, Leaf "C", Leaf "D"))

// 1

let rec probOK =
    function
    | Branch (_, prop, t1, t2) ->
        prop
        >= 0.0
        && prop <= 1.0
        && probOK t1
        && probOK t2
    | Leaf (_) -> true

probOK exp
probOK exp2

probOK exp3


// 2

let isLeaf =
    function
    | Leaf (_) -> true
    | Branch (_) -> false


let rec isSample (os, t) =
    match (os, t) with
    | head :: tail, Branch (_, _, t1, t2) ->
        match head with
        | S -> if isLeaf t1 then true else isSample (tail, t1)
        | F -> if isLeaf t2 then false else isSample (tail, t2)


isSample ([ F; S ], exp)
isSample ([ S; F ], exp)


let rec isSample2 os t =
    match os with
    | [] -> true
    | x :: xs ->
        match t with
        | Leaf (_) -> false
        | Branch (_, _, p1, p2) ->
            match x with
            | S -> isSample2 xs p1
            | F -> isSample2 xs p2

let description os t = 
    match os with 
    | head::tail -> 
                    match t with
                    | Branch (_, _, p1, p2) -> 
                    | leaf (_) ->
                    
and tree = function 
    | 