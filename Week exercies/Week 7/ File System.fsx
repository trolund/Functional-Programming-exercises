type FileSys = Element list
and Element =
| File of string * string
| Dir of string * FileSys

let d1 = Dir("d1",[File("a1","java");
        Dir("d2", [File("a2","fsx");
        Dir("d3", [File("a3","fs")])]);
        File("a4","fsx");
        Dir("d3", [File("a5","pdf")])]);;

let rec namesFileSys = function 
    | [] -> []
    | e::es -> (namesElement e) @ (namesFileSys es) 
and namesElement = function
    | File(name,ext) -> [name + "." + ext]
    | Dir(s,fs) -> s :: (namesFileSys fs)

// let rec namesElement = function
//     | [] -> []
//     | Dir(dname, file)::es -> dname::ListOfNames file @ ListOfNames es
//     | File(name, ext)::es -> (name + "." + ext)::ListOfNames es

namesElement d1

let rec searchFileSys ext = function 
    | [] -> []
    | e::es -> (searchElement ext e) @ (searchFileSys ext es) 
and searchElement ext = function
    | File(name, ex) -> if ex = ext then [name + "." + ext] else []
    | Dir(s,fs) -> searchFileSys ext fs

searchElement "pdf" d1
searchElement "fsx" d1

(*
Declare mutually recursive functions:
    longNamesFileSys: FileSys -> Set<string>
    longNamesElement: Element -> Set<string>
*)

let rec longNameFileSys =
    function
    | [] -> Set.empty<string>
    | e :: es -> Set.union (longNameElement e) (longNameFileSys es)
and longNameElement =
    function
    | File (n, ex) -> Set.ofList [n + "." + ex ]
    | Dir (d, fs) -> prependDirectory (d, longNameFileSys fs) 
and prependDirectory (dir: string, fs) =
    if Set.isEmpty fs then Set.singleton "" 
    else 
        let col = Set.minElement fs
        let f = Set.remove col fs
        Set.union (Set.ofList [dir + "/"]) (prependDirectory (dir, f))

longNameElement d1


(*
     let col = Set.minElement cols
let cols’ = Set.remove col cols
if canBeExtBy m col c
then Set.add (Set.add c col) cols’
else Set.add col (extColoring m cols’ c);;
*)

let rec longNameFileSys =
    function
    | [] -> []
    | e :: es -> (longNameElement e) @ (longNameFileSys es)

and longNameElement  =
    function
    | File (n, ex) ->  [ n + "." + ex ]
    | Dir (d, fs) -> prependDirectory d (longNameFileSys fs)

and prependDirectory dir =
    function
    | [] -> []
    | e::es -> (dir + "/" + e) :: prependDirectory dir es
