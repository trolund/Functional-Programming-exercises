module Compiler = 

    type Instruction = | ADD 
                       | SUB 
                       | SIGN 
                       | ABS 
                       | PUSH of int

    type Stack = S of int list

    let theStack = [1;2;3;4;5]

    let rec aux (s: int list, acc) = 
        match s with 
        | [] -> []
        | head::tail when tail.Length = 1 -> (head + tail.Head)::s
        | head::tail -> head::(aux (tail, acc))

    let rec aux2 s i = 
        match s with 
        | [] -> []
        | head::tail when tail.Length = 1 -> (head + tail.Head)::s
        | head::tail -> head::(aux2 tail 0)

// let rec sum p xs = List.fold (fun x acc -> if p x then x + acc else acc ) 0 xs

    let add s = S(List.fold aux2 [] s)
    
    let intpInstr s (i:Instruction) =
        match i with 
        | ADD -> add s
        | SUB -> S([0])
        | SIGN -> S([0])
        | ABS -> S([0])
        | PUSH(i) -> S([0])
     
    // let rec SIGN (s: int list) = function
    //     | [] -> []
    //     | [x]  -> -x::s
    //     | head::tail -> SIGN tail
    
 
    // let add s = 

    //     let folder (sx: int list) = 
    //         match sx with
    //         | [] -> []
    //         | [first; second]::tail -> (first + second) :: tail
    //         | _::tail -> tail

    //     List.fold folder [] s

    // add theStack

    // let intpInstr Stack = 