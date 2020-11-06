type Appliance = string
type Usage = Appliance * int

let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ ad1; ad2; ad3; ad1; ad2 ]


(*
    1. Declare a function: inv: Usage list -> bool, that checks whether all time spans
    occurring in a usage list are positive.
*)

let inv ul =
    List.forall (fun (_, span) -> span > 0) ul

inv ats

(*
    2. Declare a function durationOf: Appliance -> Usage list -> int, where the value
    of durationOf a ats is the accumulated time span appliance a is used in the list ats.
    For example, durationOf "washing machine" ats should be 4.
*)

let durationOf (a: Appliance) (ul: Usage list) =
    let rec loop acc =
        function
        | [] -> acc
        | (name, span) :: tail when name = a -> loop (acc + span) tail
        | _ :: tail -> loop acc tail

    loop 0 ul

durationOf "washing machine" ats



