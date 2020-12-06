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
                P("Emil", M, 1995, []) ]) ]) // Well-formend

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
