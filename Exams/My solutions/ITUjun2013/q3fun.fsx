type Latex<'a> =
    | Section of string * 'a * Latex<'a> * Latex<'a>
    | Subsection of string * 'a * Latex<'a> * Latex<'a>
    | Text of string * Latex<'a>
    | End

let sec =
    Section("sec", None, Section("sec2", None, End, End), End)

let text1 =
    Section
        ("Introduction",
         None,
         Text
             ("This is an introduction to ...",
              Subsection("A subsection", None, Text("As laid out in the introduction we ...", sec), sec)),
         sec)

let updateElement key (st: int list) =
    List.mapi (fun i v -> if i = key then v + 1 else v) st

updateElement 1 [ 1; 1; 1; 1; 1 ]

List.item 0 []

(updateElement 0 [ 1 ]) @ [ 1 ]


let rec listtoString l acc =
    match l with
    | [] -> acc
    | [ x ] -> listtoString [] (acc + string x)
    | x :: xs -> listtoString xs (acc + string x + ".")

listtoString [ 1; 2; 1; 1; 1 ] ""

let addSecNumbers t =

    let s (l: int list) =
        function
        | None -> listtoString l ""

    let updateElement key (st: int list) =
        List.mapi (fun i v -> if i = key then v + 1 else v) st

    let rec aux l i =
        function
        | Section (name, n, s1, s2) ->
            let pos = updateElement i l
            let possub = pos @ [ 1 ]
            Section(name, s possub n, aux possub (1 + i) s1, aux possub (1 + i) s2)
        | Subsection (name, n, s1, s2) ->
            let pos = updateElement i l
            let possub = pos @ [ 1 ]
            Subsection(name, s possub n, aux possub (1 + i) s1, aux possub (1 + i) s2)
        | Text (x, subs) -> Text(x, aux l i subs)
        | End -> End

    aux [] 1 t

addSecNumbers text1
