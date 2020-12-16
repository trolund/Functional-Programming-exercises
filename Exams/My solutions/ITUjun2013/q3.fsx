type Latex<'a> =
    | Section of string * 'a * Latex<'a>
    | Subsection of string * 'a * Latex<'a>
    | Label of string * Latex<'a>
    | Text of string * Latex<'a>
    | Ref of string * Latex<'a>
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

let addSecNumbers t =

    let updateElement key (st: int list) =
        List.mapi (fun i v -> if i = key then v + 1 else v) st

    let rec listtoString l acc =
        match l with
        | [] -> acc
        | [ x ] -> listtoString [] (acc + string x)
        | x :: xs -> listtoString xs (acc + string x + ".")

    let s (l: int list) =
        function
        | Some x -> x
        | None -> listtoString l ""

    let rec aux l i =
        function
        | Section (name, n, subs) ->
            let pos =
                if List.length l > 1 then [ List.item 0 (updateElement 0 l) ] else updateElement 0 l

            Section(name, s pos n, aux pos i subs)
        | Subsection (name, n, subs) ->
            let pos =
                updateElement 1 (if List.length l < 2 then l @ [ 0 ] else l)

            Subsection(name, s pos n, aux pos i subs)
        | Text (x, subs) -> Text(x, aux l i subs)
        | End -> End

    aux [ 0 ] 1 t

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


addSecNumbers text2 = text2'


// 3.
// Latex<'a option> -> Latex<string>


// 4.

let text3 =
    Section
        ("Introduction",
         "1",
         Label
             ("intro.sec",

              Text
                  ("In section",
                   Ref
                       ("subsec.sec",
                        Text
                            ("we describe ...",
                             Subsection
                                 ("A subsection",
                                  "1.1",
                                  Label
                                      ("subsec.sec",
                                       Text
                                           ("As laid out in the introduction, Section ",
                                            Ref("intro.sec", Text(" we ...", End))))))))))

let buildLabelEnv t =
    let rec aux cur =
        function
        | Section (_, n, subs) -> aux n subs
        | Subsection (_, n, subs) -> aux n subs
        | Label (name, subs) -> (name, cur) :: aux cur subs
        | Text (_, subs) -> aux cur subs
        | Ref (_, subs) -> aux cur subs
        | _ -> []

    Map.ofList (aux "" t)

buildLabelEnv text3


// 5.

let rec toString =
    function
    | Section (name, n, subs) -> (n + " " + name + "\n") + toString subs
    | Subsection (name, n, subs) -> (n + " " + name + "\n") + toString subs
    | Label (name, subs) -> toString subs
    | Text (name, subs) -> (name + "\n") + toString subs
    | Ref (name, subs) -> toString subs
    | _ -> ""

printf "%s" (toString text3)
