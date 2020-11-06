module Compiler =

    type Instruction =
        | ADD
        | SUB
        | SIGN
        | ABS
        | PUSH of int;;

    type Stack = S of int list;;

    (*
        TODO Part 2: Expressions: Syntax and semantics
    *)

    type Exp =     
          | X of int                   
          | C  of int             
          | Add of Exp * Exp        
          | Sub of Exp * Exp       
          | Minus of Exp * Exp
          | Abs of Exp * Exp;;

    X, C -2, C 7;;
    Abs X, Minus(C 7);;
    Add(Abs(Minus(C 7)), Sub(X, Minus(Add(C 2, X))));;

    (*
        Basic data contrucs for testing
    *)

    let theStack = S([ 1; 2; 3; 4; 5 ]);;

    let instru =
        [ PUSH 2
          PUSH 4
          PUSH 8
          SUB
          ADD
          ABS
          SIGN
          ABS ];;

    let instru2 =
        [ PUSH 2
          PUSH 4
          PUSH 10
          ADD
          ADD
          SIGN ];;
    
    let instruFail =
        [ PUSH 2
          PUSH 4
          PUSH 10
          ADD
          ADD
          ADD
          ADD
          SIGN ];;

    let ofList l = S l;;
    
    let toList = function
        | S(l) -> l;;


    let sigleEle s a =
        match s with
        | S([]) -> S([])
        | S(x :: tail) ->
            let sec = tail.Head
            let restTail = tail.Tail
            let firstelm = a sec x
            S(firstelm :: restTail);;

    let add s = sigleEle s (+);;
    let sub s = sigleEle s (/);;

    add theStack;;
    sub theStack;;

    let first s a =
        match s with
        | S([]) -> S([])
        | S(x :: tail) -> S((a x) :: tail);;

    let sign s = first s ((~-));;

    let abs s = first s (abs);;

    sign theStack;;
    abs (S([ -1; 3; 4; 5 ]));;

    let push (r: int) (s: Stack) =
        let l = toList(s)
        S(List.rev (List.append (List.rev l) [ r ]));;

    push 8 theStack;;
    push 5 (S([]));;

    let intpInstr (s: Stack) i =
        match i with
        | ADD -> add s
        | SUB -> sub s
        | SIGN -> sign s
        | ABS -> abs s
        | PUSH (x) -> push x s;;

    intpInstr (S([])) (PUSH 8);;

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
            | [], S([]) -> failwith "Sometinhg went wrong!"
            | [],S(s) -> s.Head // check if int
            | ins::tail, s -> aux tail (intpInstr s ins)

        aux i sStack;;

    exec instru;; // 2
    exec instru2;; // -16
    exec instruFail;; // will fail