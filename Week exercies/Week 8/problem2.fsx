(* 
    02157 Functional programming (Fall 2011) - Problem 2
*)

type Exp = | C of int
           | BinOp of Exp * string * Exp
           | Id of string
           | Def of string * Exp * Exp

(*
    1. Give three dierent values of type exp.

        1) 1
        2) 1 '+' 1
        3) (2 '*' 3) '+' 3
*)

let exp1: Exp = C(1)
let exp2: Exp = BinOp(C(1), "+", C(1))
let exp3: Exp = BinOp(BinOp(C(2), "*", C(3)), "+", C(3))

(*
    2. Delare a funtion toString: exp -> string, that gives a string representation
    for an expression. Put brakets around every subexpression with operators, e.g.
    (3+(5*2)) is a string representation of the above example.
*)

let exp: Exp = BinOp(BinOp(C(5), "*", C(2)), "+", C(3))

let rec toString = 
    function
    | C(x) -> string x
    | BinOp(e, s, ex) -> "(" + (toString e) + s + (toString ex) + ")"
 
toString exp

(*
    3. Delare a funtion to extrat the set of operators from an expression.
*)

let rec extratOperators = 
    function
    | C(_) -> Set.empty
    | BinOp(e, s, ex) -> Set.union (Set.union (extratOperators e) (extratOperators ex)) (Set.singleton s)

extratOperators exp

(*
    4. The type for expressions is now extended to inlude identiers (onstrutor Id) and
    loal denitions (onstrutor Def):
*)


let exp4 = Def("x", C 5 , BinOp(Id "x", "+", Id "x"))          
let exp5 = Def("x", C 5 , BinOp(Id "y", "+", Id "x")) 
(*
    Delare a funtion isDef: exp -> bool that an test whether an expression is defined.
*)

let isDef exp = 

    let rec aux env = 
        function
        | BinOp(e, s, ex) -> (aux env e) && (aux env ex)
        | Def(name, e, ex) -> 
                            let newEnv = name :: env
                            (aux newEnv e) && (aux newEnv ex)
        | Id(name) -> List.contains name env
        | _ -> true

    aux [] exp


isDef exp4
isDef exp5     



