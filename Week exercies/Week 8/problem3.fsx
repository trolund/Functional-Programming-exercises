(* Problem 3 from the exam fall 2015 *)

type Name = string

type Flow = int // can be assumed positive in below questions

type River = R of Name * Flow * Tributaries

and Tributaries = River list

(* 1. Declare F# values riv and riv3 corresponding to the rivers “R” and “R3”. *)

(*
    A river named “R” has flow 10m3/s from its source and it has three tributaries named
    “R1”, “R2” and “R3”, respectively.
    • The river “R1” has flow 5m3/s from its source and no tributaries.
    • The river “R2” has flow 15m3/s from its source and one tributary named "R4".
    • The river “R3” has flow 8m3/s from its source and no tributaries.
    • The river “R4” has flow 2m3/s from its source and no tributaries.
*)

let rev4 = R("R4", 2, [])
let rev1 = R("R1", 5, [])
let rev2 = R("R2", 15, [ rev4 ])
let rev3 = R("R3", 8, [])
let rev = R("R", 10, [ rev1; rev2; rev3 ])

(*
    2. Declare a function contains : Name → River → bool. The value of contains n r is true
    if and only if the name of r is n, or n is the name of a tributary occurring somewhere in
    r. For example, "R", "R1", "R2", "R3" and "R4" constitute all names contained in riv.
*)

let rec searchRiver n =
    function
    | [] -> false
    | e :: es -> (searchTributaries n e) || (searchRiver n es)

and searchTributaries n =
    function
    | R (name, _, tri) -> name = n || searchRiver n tri

and contains n r = searchRiver n [ r ]

contains "R4" rev

(*
    3. Declare a function allNames r which returns a list with all names contained in the river
    r. The order in which names occur in the list is of no significance.
*)


let rec namesRiverSys =
    function
    | [] -> []
    | e :: es -> (allNames e) @ (namesRiverSys es)

and allNames =
    function
    | R (name, _, tri) -> name :: (namesRiverSys tri)

allNames rev

(*
    4. Declare a function totalFlow r which returns the total flow in the river mouth (in
    Danish ‘udmunding’) of r, by adding the flow from the source of r to the total flows of
    r’s tributaries. For example totalFlow riv = 40.
*)

let rec subTotal =
    function
    | [] -> 0
    | e :: es -> (totalFlow e) + (subTotal es)

and totalFlow =
    function
    | R (_, flow, tri) -> flow + (subTotal tri)

totalFlow rev

(*
    5. Declare a function mainSource : River → (Name ∗ Flow). If (n, fl) = mainSource r,
    then fl is the biggest flow of some source occurring in the river r and n is the name of
    a river having this “biggest” source. For example, mainSource riv = ("R2",15) and
    mainSource riv3 = ("R3",8).
*)

let mainSource r =

    let rec subSource =
        function
        | [] -> []
        | e :: es -> (source e) @ (subSource es)

    and source =
        function
        | R (name, flow, tri) -> (name, flow) :: (subSource tri)

    let folder acc elem =
        let (_, aFlow) = acc
        let (_, cFlow) = elem

        if cFlow > aFlow then elem else acc

    List.fold folder ("", 0) (source r)

mainSource rev
mainSource rev3

(*
    6. Declare a function tryInsert : Name → River → River → River option. The value
    of tryInsert n t r is Some r'
    if n is the name of a river in r and r'
    is obtained from r
    by adding t as a tributary of n. The value of tryInsert n t r is None if n is not a name
    occurring in r.
*)


let rec tryInsert (n: Name) (t: River) =
    function
    | R (n', fl, ts) when n = n' -> Some(R(n', fl, t :: ts))
    | R (n', fl, ts) ->
        match tryInsertTriInList n t ts with
        | None -> None
        | Some ts' -> Some(R(n', fl, ts'))

and tryInsertTriInList n t =
    function
    | [] -> None
    | r :: ts ->
        match tryInsert n t r with
        | None ->
            match tryInsertTriInList n t ts with
            | None -> None
            | Some ts' -> Some(r :: ts')
        | Some r' -> Some(r' :: ts)

let tri = R("R5", 2, [])
tryInsert "R3" tri rev

(*
    7. Discuss briefly possible limitations of the above tree-based model of rivers.

    Svar: 
    modelen viser ikke noget om fx. længden af forbindelser eller pladsering af disse forbindelser i forhold til hinaden
*)