type T<'a> = N of 'a * T<'a> list

type Path = int list

let td = N("g", [])
let tc = N("c", [ N("d", []); N("e", [ td ]) ])
let tb = N("b", [ N("c", []) ])
let ta = N("a", [ tb; tc; N("f", []) ])

let path =
    [ 1
      1
      0 ] // path to td in ta (angivet i opgave beskrivelse)

// iterate over to lists
let rec getChildren =
    function
    | [], children -> children
    | head :: tail, children -> [ get tail (List.item head children) ]

and get (path: Path) =
    function
    | N (name, children) -> N(name, getChildren (path, children))


get path ta
