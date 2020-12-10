type Titel = string

type Section = Titel * Elem list

and Elem =
    | Par of string
    | Sub of Section

type Chapter = Titel * Section list
type Book = Chapter list

let sec11 =
    ("Background",
     [ Par "bla"
       Sub(("Why programming", [ Par "Bla." ])) ])

let sec12 =
    ("An example",
     [ Par "bla"
       Sub(("Special features", [ Par "Bla." ])) ])

let sec21 =
    ("Fundamental concepts",
     [ Par "bla"
       Sub(("Mathematical background", [ Par "Bla." ])) ])

let sec22 =
    ("Operational semantics",
     [ Sub(("Basics", [ Par "Bla." ]))
       Sub(("Applications", [ Par "Bla." ])) ])

let sec23 = ("Further reading", [ Par "bla" ])
let sec31 = ("Overview", [ Par "bla" ])
let sec32 = ("A simple example", [ Par "bla" ])
let sec33 = ("An advanced example", [ Par "bla" ])
let sec41 = ("Status", [ Par "bla" ])
let sec42 = ("What's next?", [ Par "bla" ])
let chl = ("Introduction", [ sec11; sec12 ])

let ch2: Chapter =
    ("Basic Issues", [ sec21; sec22; sec23 ])

let ch3 =
    ("Advanced Issues", [ sec31; sec32; sec33 ])

let ch4 = ("Conclusion", [ sec41; sec42 ])
let booki = [ chl; ch2; ch3; ch4 ]

// 1

let maxL l =

    let rec loop m =
        function
        | [] -> m
        | head :: tail -> if head > m then loop head tail else loop m tail

    loop 0 l

maxL []

maxL [ 0; 4; 6; 10 ]

// 2

let rec overview =
    function
    | Book (chapters) -> section chapters
    | Chapter (titel, sections) -> titel :: section sections

and section =
    function
    | [] -> []
    | head :: tail -> overview head :: section tail
