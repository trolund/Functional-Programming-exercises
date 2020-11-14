type Instruction =
    | ADD
    | SUB
    | SIGN
    | ABS
    | PUSH of int

type Stack = S of int list

type Exp =
    | X
    | C of int
    | Add of Exp * Exp
    | Sub of Exp * Exp
    | Minus of Exp
    | Abs of Exp

let e1 = X
let e11 = C -2
let e12 = C 7
let e2 = Abs X, Minus(C 7)

let e3 =
    Add(Abs(Minus(C 7)), Sub(X, Minus(Add(C 2, X))))

(*
            Basic data contrucs for testing
        *)

let theStack = S([ 1; 2; 3; 4; 5 ])

let instru: Instruction list =
    [ PUSH 2
      PUSH 4
      PUSH 8
      SUB
      ADD
      ABS
      SIGN
      ABS ]

let instru2 =
    [ PUSH 2
      PUSH 4
      PUSH 10
      ADD
      ADD
      SIGN ]

let instruFail =
    [ PUSH 2
      PUSH 4
      PUSH 10
      ADD
      ADD
      ADD
      ADD
      SIGN ]

let ofList l = S l

let toList =
    function
    | S (l) -> l


let sigleEle s a =
    match s with
    | S ([]) -> S([])
    | S (x :: tail) ->
        let sec = tail.Head
        let restTail = tail.Tail
        let firstelm = a sec x
        S(firstelm :: restTail)

let add s = sigleEle s (+)
let sub s = sigleEle s (/)

add theStack
sub theStack

let first s a =
    match s with
    | S ([]) -> S([])
    | S (x :: tail) -> S((a x) :: tail)

let sign s = first s ((~-))

let abs2 s = first s (abs)

sign theStack
abs2 (S([ -1; 3; 4; 5 ]))

let push (r: int) (s: Stack) =
    let l = toList (s)
    S(List.rev (List.append (List.rev l) [ r ]))

push 8 theStack
push 5 (S([]))

let intpInstr (s: Stack) i =
    match i with
    | ADD -> add s
    | SUB -> sub s
    | SIGN -> sign s
    | ABS -> abs2 s
    | PUSH (x) -> push x s

intpInstr (S([])) (PUSH 8)

(*
            A program for the stack machine is a list of instructions [i1, i2, . . . , in]. A program is executed
            by executing the instructions i1, i2, . . . , in one after the other, in that order, starting with an
            empty stack. The result of the execution is the top value of the stack when all instructions
            have been executed.

            Declare an F# function to interpret the execution of a program:

                exec: Instruction list -> int
        *)

let rec exec i =
    let sStack = S([])

    let rec aux (i: Instruction list) (s: Stack) =
        match (i, s) with
        | [], S ([]) -> failwith "Sometinhg went wrong!"
        | [], S (s) -> s.Head // check if int
        | ins :: tail, s -> aux tail (intpInstr s ins)

    aux i sStack

exec instru // 2
exec instru2

let rec sem e x =
    match e with
    | X -> x
    | C (v) -> v
    | Add (a, b) -> (sem a x) + (sem b x)
    | Sub (a, b) -> (sem b x) / (sem a x)
    | Minus (v) -> -(sem v x)
    | Abs (v) -> abs (sem v x)

sem e1 2
sem e3 2
sem e3 -120

// compile: Exp -> int -> Instruction list

let compile e x =
    let rec auxCompile e (tempS: Instruction list) =
        match e with
        | X -> (PUSH x) :: tempS
        | C (v) -> (PUSH v) :: tempS
        | Add (a, b) -> (auxCompile b tempS) @ (auxCompile a tempS) @ [ADD] 
        | Sub (a, b) -> (auxCompile b tempS) @ (auxCompile a tempS) @ [SUB] 
        | Minus (v) -> (auxCompile v tempS) @ [SIGN]
        | Abs (v) -> (auxCompile v tempS) @ [ABS]

    auxCompile e []

let compiled = compile e3 2
exec compiled

(* TODO -> gennemgå optimeringer! ABS helt sikkert ikke rigtig *)

let compileOptimized e x =
    let rec auxCompile e (tempS: Instruction list) =
        match e with
        | X -> (PUSH x) :: tempS
        | C (v) -> (PUSH v) :: tempS
        | Add (a, b) -> match a, b with 
                        | a, C 0 -> auxCompile a tempS
                        | C 0, b -> auxCompile b tempS
                        | C a', C b' -> (PUSH (a' + b')) :: tempS
                        | a, b -> (auxCompile b tempS) @ (auxCompile a tempS) @ [ADD] 
        | Sub (a, b) -> match a, b with 
                        | a, C 0 -> auxCompile a tempS
                        | C 0, b -> auxCompile (Minus b) tempS
                        | C a', C b' -> (PUSH (a' - b')) :: tempS
                        | a, b -> (auxCompile b tempS) @ (auxCompile a tempS) @ [SUB] 
        | Minus (v) -> match v with 
                        | C a' -> (PUSH (-a')) :: tempS
                        | Minus e -> (auxCompile e tempS) @ tempS
                        | a' -> (auxCompile a' tempS) @ [SIGN]
        | Abs (v) -> match v with 
                     | C a' -> (PUSH (abs a')) :: tempS
                     | Minus a' -> auxCompile a' tempS @ [ABS]
                     | Abs a' -> auxCompile a' tempS @ [ABS]
                     | a' -> (auxCompile a' tempS) @ [ABS]

    auxCompile e []

let compileOptimizedres = compileOptimized e3 2
compileOptimizedres
exec compileOptimizedres

exec (compileOptimized e3 1)
