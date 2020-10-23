type Yb = int // yb is the year of birth

type Interests = 
    | Jazz
    | Golf
    | CS
    | Soccer

type Ths = Interests list

type No = string // phone number

type MemDescription = No * Yb * Ths

type Member = {
    name : string
    description: MemDescription
}

type Predicate = MemDescription -> bool

type Register = Member list

// type Club = MemDescription list
type Arrangement = Predicate * Register -> MemDescription list

(* 1) A declaration of a register reg, a declaration of an arrangement p1 for the above described arrangement p1, 
and a declaration of an arrangement p2 that is directed to young club MemDescriptionbers that are interested in either ”soccer” or ”jazz” or both. 
These declarations should be constructed so that they can serve as illustrative examples. *)

let MemDescription1 = { Member.name = "Troels"; Member.description = ("29456661", 1994, [ Interests.Jazz ])} : Member

let MemDescription2 =
   { Member.name = "Emil";  Member.description = ("29456660", 1995, [ Interests.Jazz; Interests.CS; Interests.Golf; Interests.Soccer ])}

let MemDescription3 = { Member.name = "Morten"; Member.description = ("29456660", 1995, [ Interests.CS; Interests.Golf ])}


let rec p1 (MemDescription: MemDescription) =
    let No, Yb, Ths = MemDescription;
    match Ths with
    | [] -> false
    | x :: tail ->
        x = Interests.Jazz && Yb > 1982
        && p1 (No, Yb, tail)

let rec p2 (MemDescription: MemDescription) =
    let No, Yb, Ths = MemDescription;
    match Ths with
    | [] -> false
    | x :: tail ->
        x = Interests.Jazz
        || x = Interests.Soccer && Yb > 1982
        || p2 (No, Yb, tail)

p2 MemDescription1.description;;

p2 MemDescription2.description;;

p2 MemDescription3.description;;

p1 MemDescription2.description;;

p1 MemDescription3.description;;

let reg: Register = [MemDescription1; MemDescription2; MemDescription3]

(* 2) A declaration of a function extractTargetGroup p r that gives a list with names and phone numbers of the MemDescriptionbers in register r that may be interested in the arrangement p. 
State the type of extractTargetGroup in a comment. Make use of the type names introduced under point 1. above, so that the type reflects the intention with the function.
Tests of extractTargetGroup involving reg, p1 and p2. *)

let extractTargetGroup (p: Predicate, r: Register) = 
    let res = List.map (fun item -> item.description) r
    List.filter p res

extractTargetGroup (p2, reg)
extractTargetGroup (p1, reg)