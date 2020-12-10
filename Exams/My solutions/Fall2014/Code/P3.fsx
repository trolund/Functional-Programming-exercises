type T<'a> = N of 'a * T<'a> list


let rec f (N (e, es)) = e :: g es

and g =
    function
    | [] -> []
    | e :: es -> f e @ g es


let rec h p t =
    match t with
    | N (e, _) when p e -> N(e, [])
    | N (e, es) -> N(e, List.map (h p) es)

let rec k (N (_, es)) = 1 + List.fold max 0 (List.map k es)

// 1

let t =
    N("a", [ N("b", []); N("c", []); N("d", []) ])

let t2 = N("a", [])

let t3 = N("a", [ N("b", []); N("c", []) ])

// 2

// f
// T<'a> -> 'a list
// list of all 'a s in the tree

// g
// T<'a> list -> 'a list
// list of all 'a in a list of trees

// h
// ('a -> bool) -> T<'a> ->  T<'a>
// traverses the three until the predicate is met and returns that traversed tree

// k
// T<'a> -> int
// Calcualtes the depth of the three
