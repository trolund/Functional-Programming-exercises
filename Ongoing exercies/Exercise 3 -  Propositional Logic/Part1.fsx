#I "/Users/troelslund/.nuget/packages/fscheck/3.0.0-alpha4/lib/net452"
#r "FsCheck.dll"
open FsCheck

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

    // test props:

    let prop = Dis(A "3", A "1")
    let prop2 = Dis(A "3", Con(A "3", Neg(A "4")))
    let prop3 = Con(A "3", A "1")
    let prop4 = Neg(Con(A "1", A "2"))

    (*
        2.  Declare a functionsem: Prop<’a> -> Set<’a> -> bool
    *)

    let rec sem p asg =
        match asg with
        | A (a) -> a = p
        | Neg (a) -> not (sem p a)
        | Con (a, b) -> (sem p a) && (sem p b)
        | Dis (a, b) -> (sem p a) || (sem p b)


    sem "1" prop
    sem "3" prop2
    sem "1" prop3

    (*
        Part 2: Negation Normal Form
    *)

    (*

        3.  Declare functiontoNnfptransforming a propositionpinto an equivalent propositionin negation normal form,
        using the de Morgan laws:

            ¬(P∧Q)   is equivalent to   (¬P)∨(¬Q)
            ¬(P∨Q)   is equivalent to   (¬P)∧(¬Q)

        and the law:¬(¬P) is equivalent to P.

    *)

    let rec toNnf =
        function
        | Neg (Neg (a)) -> a
        | Neg (Con (a, b)) -> toNnf (Dis(Neg(toNnf a), Neg(toNnf b)))
        | Neg (Dis (a, b)) -> toNnf (Con(Neg(toNnf a), Neg(toNnf b)))
        | Con (a, b) -> Con(toNnf a, toNnf b)
        | Dis (a, b) -> Dis(toNnf a, toNnf b)
        | p -> p

    toNnf prop4

    (*
        4.  First, declare a functiononNnf: Prop<’a> -> boolthat can decide whether a propo-sition is on negation normal form,
        and use property-based testing to validate thattoNnfpis on negation normal form for propositionspof typeProp<string>
    *)

    let rec onNnf =
        function
        | Neg (Neg (_)) -> false
        | Neg (Con (_)) -> false
        | Neg (Dis (_)) -> false
        | Con (a, b) -> onNnf a && onNnf b
        | Dis (a, b) -> onNnf a || onNnf b
        | _ -> true

    onNnf (toNnf prop4)
    onNnf prop4

    // property testing

    let test exp =
        (sem "1" (toNnf prop4)) = (sem "1" prop4)

    Check.Quick test


    (*
    5.  Declare a typeFinitehavingnvalues.
    Choosensmall; but be aware that it shouldbe meaningful to letFsCheck.Check.Quickgenerate 100 random assignments.
*)

    type Finite =
        | S of string
        | I of int

(*
    6. Use property-based testing to validate that p is equivalent to toNnf p, where p :
    Prop<Finite>.
*)
