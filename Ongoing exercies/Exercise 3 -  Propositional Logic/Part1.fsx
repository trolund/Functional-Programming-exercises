(*
    Part 1: Abstract syntax and semantics

*)

module Part1 =

    (*
        1.  Declare a typeProp<’a>for propositions so that
    *)


    type Prop<'a> =
        | A of 'a
        | Dis of Prop<'a> * Prop<'a>
        | Con of Prop<'a> * Prop<'a>
        | Neg of Prop<'a>


    (*
        2.  Declare a functionsem: Prop<’a> -> Set<’a> -> bool
    *)

    let rec sem p asg =
        match asg with
        | A (a) -> a = p
        | Neg (a) -> not (sem p a)
        | Con (a, b) -> (sem p a) && (sem p b)
        | Dis (a, b) -> (sem p a) || (sem p b)
