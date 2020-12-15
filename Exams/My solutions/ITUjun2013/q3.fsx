type Latex<'a> =
    | Section of string * 'a * Latex<'a>
    | Subsection of string * 'a * Latex<'a>
    | Text of string * Latex<'a>
    | End


let text1 =
    Section
        ("Introduction",
         None,
         Text
             ("This is an introduction to ...",
              Subsection("A subsection", None, Text("As laid out in the introduction we ...", End))))


// 1.

(*
    Latex<'a option>
*)

// 2.

let text2 =
    Section
        ("Introduction",
         None,
         Text
             ("This is an introduction to ...",
              Subsection
                  ("A subsection",
                   None,
                   Text
                       ("As laid out in the introduction we ...",
                        Subsection
                            ("Yet a subsection",
                             None,
                             Section("And yet a section", None, Subsection("A subsection more...", None, End)))))))



let rec listtoString l acc =
    match l with
    | [] -> acc
    | [ x ] -> listtoString [] (acc + string x)
    | x :: xs -> listtoString xs (acc + string x + ".")

listtoString [ 1; 2; 1; 1; 1 ] ""

let addSecNumbers t =

    let updateElement (st: int list) =
        List.mapi (fun i v -> if i = ((List.length st) - 1) then v + 1 else v) st

    let s (l: int list) =
        function
        | Some x -> x
        | None -> listtoString l ""

    let rec aux l i =
        function
        | Section (name, n, subs) ->
            let possub = l @ [ 0 ]
            let pos = updateElement possub
            Section(name, s pos n, aux pos (1 + i) subs)
        | Subsection (name, n, subs) ->
            let possub = l @ [ 0 ]
            let pos = updateElement possub
            Subsection(name, s pos n, aux pos (1 + i) subs)
        | Text (x, subs) -> Text(x, aux l i subs)
        | End -> End

    aux [] 0 t

addSecNumbers text1
addSecNumbers text2


let text2' =
    Section
        ("Introduction",
         "1",
         Text
             ("This is an introduction to ...",
              Subsection
                  ("A subsection",
                   "1.1",
                   Text
                       ("As laid out in the introduction we ...",
                        Subsection
                            ("Yet a subsection",
                             "1.2",
                             Section("And yet a section", "2", Subsection("A subsection more...", "2.1", End)))))))


addSecNumbers text2
