List.fold (fun acc x -> x :: acc) [] [ 1; 2; 3; 4; 5 ]

List.foldBack (fun x acc -> x :: acc) [ 1; 2; 3; 4; 5 ] []

List.foldBack (fun x acc -> x + 1 :: acc) [ 1; 2; 3; 4; 5 ] []

List.fold (fun acc x -> x :: acc) [] [ 1; 2; 3; 4; 5 ]

let testList = [ 1; 2; 3; 4; 5 ]

let folder state value =
    if value % 2 = 0 then state + value else state

let initState = 0

List.fold folder initState testList

List.foldBack folder testList initState
