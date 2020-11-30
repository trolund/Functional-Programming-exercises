type Name = string

type Flow = int // can be assumed positive in below questions

type River = R of Name * Flow * Tributaries

and Tributaries = River list

let riv3 = R("R3", 8, [])
let riv1 = R("R1", 5, [])
let riv4 = R("R4", 2, [])
let riv2 = R("R2", 15, [ riv4 ])

let riv = R("R", 10, [ riv1; riv2; riv3 ])


let rec triSearch n =
    function
    | [] -> false
    | head :: tail -> contains n head || triSearch n tail

and contains n =
    function
    | R (name, _, tri) when name = n -> true
    | R (_, _, tri) -> triSearch n tri


contains "R10" riv


let rec nameSearch =
    function
    | [] -> []
    | head :: tail -> allNames head @ nameSearch tail

and allNames =
    function
    | R (name, _, tri) -> name :: nameSearch tri

allNames riv


let rec subFlow =
    function
    | [] -> 0
    | head :: tail -> (totalFlow head) + (subFlow tail)

and totalFlow =
    function
    | R (_, flow, tri) -> flow + (subFlow tri)

totalFlow riv
