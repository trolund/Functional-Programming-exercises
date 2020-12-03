type Tree<'a> =
    | Lf
    | Br of Tree<'a> * 'a * Tree<'a>

let t =
    Br(Br(Br(Lf, 1, Lf), 2, Br(Lf, 3, Lf)), 4, Br(Br(Lf, 5, Lf), 6, Br(Lf, 7, Lf)))

let rec reflect =
    function
    | Lf -> Lf
    | Br (a, v, b) -> Br(reflect b, v, reflect a)

t
reflect t

let accumulate t =
    let rec loop acc =
        function
        | Lf -> Lf
        | Br (a, v, b) ->
            let nodeValue = acc + v
            Br(loop nodeValue a, nodeValue, loop nodeValue b)

    loop 0 t

accumulate t

let rec k i t =
    match t with
    | Lf -> Lf
    | Br (tl, a, tr) -> Br(k (i * i) tl, i * a, k (i * i) tr)

let rec h n m =
    function
    | Br (tl, a, tr) when n = m -> h n 1 tl @ [ a ] @ h n 1 tr
    | Br (tl, _, tr) -> h n (m + 1) tl @ h n (m + 1) tr
    | Lf -> []

h 0 0 t
// ekstra (just for fun)
// let depth t =
//     let rec loop acc =
//         function
//         | Lf -> acc
//         | Br (a, v, b) ->
//             let subA = loop (acc) a
//             let subB = loop (acc) b
//             if subA > subB then subA + 1.0 else subB + 1.0
//     loop 0.0 t
// let t2 = Br(Br(Lf, 1, Lf), 2, Br(Lf, 3, Lf))
// depth t
// depth t2
// let toString t =
//     let numOfXTabs = depth t
//     let space numOfTabs =
//         let rec loop acc num =
//             match num with
//             | num when num < 0.25 -> acc
//             | _ -> loop (" " + acc) (num / 2.0)
//         loop "" numOfTabs
//     let rec loop acc w =
//         function
//         | Lf -> acc
//         | Br (a, v, b) ->
//             "_"
//             + loop (acc + (space (w + 2.0)) + string (v)) (w / 2.0) a
//             + loop (acc + (space w) + string (v)) (w / 2.0) b
//             + "\n\n"
//     loop "" numOfXTabs t
// printf "%s" (toString t)
