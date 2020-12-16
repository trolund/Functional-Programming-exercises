type FileSys = Element list

and Element =
    | File of string * string
    | Dir of string * FileSys

let d1 =
    Dir
        ("d1",
         [ File("a1", "java")
           Dir
               ("d2",
                [ File("a2", "fsx")
                  Dir("d3", [ File("a3", "fs") ]) ])
           File("a4", "fsx")
           Dir("d3", [ File("a5", "pdf") ]) ])



let rec namesFileSys =
    function
    | [] -> []
    | x :: xs -> namesElement x @ namesFileSys xs

and namesElement =
    function
    | File (name, ext) -> [ (name + "." + ext) ]
    | Dir (name, sub) -> name :: namesFileSys sub

namesElement d1




let rec searchFileSys ext =
    function
    | [] -> []
    | x :: xs -> searchElement ext x @ searchFileSys ext xs

and searchElement ext =
    function
    | File (name, text) -> if ext = text then [ name ] else []
    | Dir (name, sub) -> searchFileSys ext sub

searchElement "fsx" d1




let rec longNamesFileSys =
    function
    | [] -> []
    | x :: xs -> longNamesElement x @ longNamesFileSys xs

and longNamesElement =
    function
    | File (name, ext) -> [ name + "." + ext ]
    | Dir (name, sub) ->
        printfn "%A" (longNamesFileSys sub)
        List.foldBack (appendDirectory name) (longNamesFileSys sub) []

and appendDirectory =
    fun dir name dSet -> dSet @ [ dir + "/" + name ]

longNamesElement d1
