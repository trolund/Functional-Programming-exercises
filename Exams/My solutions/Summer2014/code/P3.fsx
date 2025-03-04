type Name = string

type Sex =
    | M // male
    | F // female

type YearOfBirth = int

type FamilyTree = P of Name * Sex * YearOfBirth * Children

and Children = FamilyTree list

let myTree =
    P
        ("Frede",
         M,
         1800,
         [ P
             ("Mor",
              F,
              1830,
              [ P("Troels", M, 1994, [])
                P("Emil", M, 1995, []) ])
           P("Hennig", M, 1840, [])
           P("Flemming", M, 1850, [])
           P("Gustav", M, 1960, []) ]) // Well-formend

let myTree2 =
    P
        ("Frede",
         M,
         1800,
         [ P
             ("Mor",
              F,
              1830,
              [ P("Troels", M, 1995, [])
                P("Emil", M, 1994, []) ]) ]) // NOT Well-formend

let myTree3 =
    P
        ("Frede",
         M,
         1850,
         [ P
             ("Mor",
              F,
              1830,
              [ P("Troels", M, 1994, [])
                P("Emil", M, 1995, []) ]) ]) // NOT Well-formend

let rec isWFchildren lastCBith pb =
    function
    | [] -> true
    | child :: rest ->
        let (P (_, _, yofb, c)) = child
        pb < yofb
        && lastCBith < yofb
        && isWF child
        && isWFchildren yofb pb rest

and isWF =
    function
    | P (n, s, yofb, c) -> isWFchildren 0 yofb c


isWF myTree
isWF myTree2
isWF myTree3

let makePerson (n, s, yofb) = P(n, s, yofb, [])


let rec insertChildOf n c t =
    match t with
    | P (name, s, yofb, cs) when name = n -> Some(P(n, s, yofb, insertInOrder c cs))
    | P (n, s, yofb, cs) ->
        match insertChildOfInList n c cs with
        | None -> None
        | Some cs' -> Some(P(n, s, yofb, cs'))

and insertChildOfInList n c cs =
    match cs with
    | [] -> None
    | c :: cs ->
        match insertChildOf n c c with
        | None ->
            match insertChildOfInList n c cs with
            | None -> None
            | Some cs' -> Some(c :: cs')
        | Some c' -> Some(c' :: cs)

and insertInOrder c cs =
    match cs with
    | [] -> []
    | curP :: tail ->
        let (P (_, _, pyofb, cs)) = c
        let (P (_, _, cyofb, cs)) = curP
        if cyofb < pyofb then curP :: (insertInOrder c tail) else c :: curP :: tail

let newtree =
    insertChildOf "Frede" (P("Finn", M, 1956, [])) myTree

let unpackOption =
    function
    | Some (v) -> v
    | None -> failwith "Ingen værdi fundet....."

isWF (unpackOption newtree)


let newtree2 =
    insertChildOf "Frede" (P("Martin", M, 1820, [])) myTree

isWF (unpackOption newtree2)


let rec find n =
    function
    | P (name, s, yofb, cs) when n = name -> [ (name, s, yofb, getChildrenInfo cs) ]
    | P (_, _, _, cs) -> findinlist n cs

and findinlist n cs =
    match cs with
    | [] -> []
    | head :: tail -> find n head @ findinlist n tail

and getChildrenInfo =
    function
    | [] -> []
    | head :: tail ->
        let (P (name, s, yofb, cs)) = head
        (name, s, yofb) :: getChildrenInfo tail

find "Mor" myTree


let rec toString (n: int) (t: FamilyTree) =
    match t with
    | P (name, s, yofb, cs) ->
        name
        + " "
        + (sexToString s)
        + " "
        + string (yofb)
        + "\n"
        + toStringList (n + n) cs
    | _ -> failwith "Er ikke et rigtigt stamtræ"

and toStringList n =
    function
    | [] -> ""
    | head :: list ->
        space n
        + toString n head
        + (toStringList (n) list)

and sexToString =
    function
    | M -> "Male"
    | F -> "Female"

and space n =

    let rec loop acc n =
        match n with
        | 0 -> acc
        | _ -> loop (acc + " ") (n - 2)

    loop "" n

printf "%s" (toString 6 myTree)


let rec truncate =
    function
    | P (name, s, yofb, cs) when s = F -> P(name, s, yofb, [])
    | P (name, s, yofb, cs) -> P(name, s, yofb, truncateList cs)

and truncateList =
    function
    | [] -> []
    | head :: tail -> truncate head :: truncateList tail

printf "%s" (toString 6 (truncate myTree))
