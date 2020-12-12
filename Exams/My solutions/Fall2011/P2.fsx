(* Problem 2 (30%) *)

type Exp =
    | C of int
    | BinOp of Exp * string * Exp
    | Id of string
    | Def of string * Exp * Exp

// 1.

let exp1 = C 1
let exp2 = BinOp(C 2, "+", C 5)
let exp3 = BinOp(C 2, "+", BinOp(C 4, "*", C 6))

let exp4 =
    BinOp(BinOp(C 10, "+", BinOp(C 1, "*", C 4)), "/", BinOp(C 4, "*", BinOp(C 2, "+", BinOp(C 4, "*", C 6))))

// 2.

let rec tostring =
    function
    | C (v) -> string (v)
    | BinOp (e1, op, e2) -> "(" + tostring e1 + op + tostring e2 + ")"

tostring exp3
// 3.

let extract e =

    let rec loop =
        function
        | C (v) -> []
        | BinOp (e1, op, e2) -> loop e1 @ [ op ] @ loop e2

    Set.ofList (loop e)

extract exp3
extract exp4


// 4.

let isDef e =

    let rec aux env =
        function
        | Id (s) -> List.contains s env
        | Def (n, e1, e2) -> aux (n :: env) e1 && aux (n :: env) e2
        | C (_) -> true
        | BinOp (e1, _, e2) -> aux env e1 && aux env e2

    aux [] e

let e1 =
    Def("x", C 5, BinOp(Id "x", "+", Id "x")) // idDef = true

let e2 =
    Def("x", C 5, BinOp(Id "y", "+", Id "x")) // idDef = false

isDef e1
isDef e2
