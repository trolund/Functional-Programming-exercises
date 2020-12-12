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
let sec34 = ("Summary", [ Par "bla" ])
let sec41 = ("Status", [ Par "bla" ])
let sec42 = ("What's next?", [ Par "bla" ])
let chl = ("Introduction", [ sec11; sec12 ])

let ch2: Chapter =
    ("Basic Issues", [ sec21; sec22; sec23 ])

let ch3 =
    ("Advanced Issues", [ sec31; sec32; sec33; sec34 ])

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
    | [] -> []
    | head :: tail ->
        let (t, c) = head
        t :: overview tail

overview booki


// let rec depthSection (x: Section) =
//     match x with
//     | (titel, elist) -> 1 + List.fold max 0 (depthElemList elist)

// and depthElemList =
//     function
//     | [] -> []
//     | head :: tail -> depthElem head :: depthElemList tail

// and depthElem =
//     function
//     | Par (t) -> 0
//     | Sub (s) -> 1 + depthSection s

// and depthChapter (x: Chapter list) =
//     match x with
//     | [] -> []
//     | (titel, sections) :: tail -> depthSectionList sections @ depthChapter tail

// and depthSectionList =
//     function
//     | [] -> []
//     | head :: tail -> depthSection head :: depthSectionList tail

// and depthBook book = List.fold max 0 (depthChapter book)

// depthBook booki

// let rec depthSection = function TODO
//     | [] -> 0
//     | (_, elist) ->
// and elementList = function
//     | Par(_) ->
//     | Sub(_, s) ->

let tocB b =

    let rec loopElem cn sn ssn l =
        match l with
        | Par (_) -> []
        | Sub (t, s) -> ([ cn; sn; ssn ], t) :: loopElemList cn sn ssn s

    and loopElemList cn sn ssn =
        function
        | [] -> []
        | head :: tail ->
            loopElem cn sn ssn head
            @ loopElemList cn sn ssn tail

    let rec loopSection cn sn (l: Section list) =
        match l with
        | [] -> []
        | (t, el) :: tail ->
            ([ cn; sn ], t)
            :: loopElemList cn cn sn el
            @ loopSection cn (sn + 1) tail

    let rec loopChapter cn =
        function
        | [] -> []
        | head :: tail ->
            let (t, s) = head
            ([ cn ], t)
            :: loopSection cn 1 s
            @ loopChapter (cn + 1) tail

    loopChapter 1 b

tocB booki
