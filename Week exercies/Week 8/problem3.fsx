(* Problem 3 from the exam fall 2015 *)

type Name = string
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
and Tributaries = River list