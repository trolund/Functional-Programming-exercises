// Problem 1, questions 6 from Exam, Fall 2013

let inv x = Map.forall (fun _ v -> v > 0) x

// test
inv (Map.ofList [ "a", 2; "a", 4 ])
inv (Map.ofList [ "a", 2; "a", -4 ])

let insert (k, v) m: Map<string, int> =
    let res = Map.tryFind k m
    match res with
    | Some oldv -> m.Add(k, v + oldv)
    | None -> m.Add(k, v)


let testMap = Map.ofList [ "a", 2; "b", -4 ]

insert ("a", 2) testMap
// let rec extColoring m cols c =
//     if Set.isEmpty cols
//     then Set.singleton (Set.singleton c)
//     else let col = Set.minElement cols
//     let cols’ = Set.remove col cols
//     if canBeExtBy m col c
//     then Set.add (Set.add c col) cols’
//     else Set.add col (extColoring m cols’ c);;
// let rec union (m1, m2) = if Set.isEmpty m1 then Set.singleton m1






