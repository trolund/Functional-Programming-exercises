type Yb = int // yb is the year of birth

type Ths = string list

type No = string // phone number

type Mem = No * Yb * Ths
type Predicate = Mem -> bool

type Club = Mem list
type Arrangement = Predicate * Club -> Mem list

let mem1: Mem = ("29456661", 1994, [ "jazz" ])

let mem2: Mem =
    ("29456660", 1995, [ "jazz"; "cs"; "golf"; "soccer" ])

let mem3: Mem = ("29456660", 1995, [ "cs"; "golf" ])

let rec p2 (No, Yb, Ths) =
    match Ths with
    | [] -> false
    | x :: tail ->
        x = "jazz"
        || x = "soccer" && Yb > 1982
        || p2 (No, Yb, tail)

p2 mem1

p2 mem2

p2 mem3
