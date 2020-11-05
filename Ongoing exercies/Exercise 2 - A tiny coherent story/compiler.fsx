module Compiler = 

    type Instruction = | ADD 
                       | SUB 
                       | SIGN 
                       | ABS 
                       | PUSH of int

    type Stack = S of int list

    let theStack = [1;2;3;4;5]

    let sigleEle s a = 
        match s with 
        | [] -> S([])
        | x::tail -> let sec = tail.Head
                     let restTail = tail.Tail
                     let firstelm = a sec x
                     S(firstelm :: restTail)

    let add s = sigleEle s (+)
    let sub s = sigleEle s (-)
    
    add theStack
    sub theStack

    let first s a =
        match s with 
        | [] -> S([])
        | x::tail -> S((a x)::tail)
     
    let sign s = first s (( ~- ))

    let abs s = first s (abs)

    sign theStack
    abs [-1;3;4;5]

    let push (r:int) s = S(List.rev (List.append (List.rev s) [r]))

    push 8 theStack

    let intpInstr s (i:Instruction) =
        match i with 
        | ADD -> add s
        | SUB -> sub s
        | SIGN -> sign s
        | ABS -> abs s
        | PUSH(i) -> push i s
    
