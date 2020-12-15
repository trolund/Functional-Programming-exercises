List.fold (fun acc x -> x :: acc) [] [ 1; 2; 3; 4; 5 ]

List.foldBack (fun x acc -> x :: acc) [ 1; 2; 3; 4; 5 ] []

List.foldBack (fun x acc -> x + 1 :: acc) [ 1; 2; 3; 4; 5 ] []

List.fold (fun acc x -> x :: acc) [] [ 1; 2; 3; 4; 5 ]

let testList = [ 1; 2; 3; 4; 5 ]

let folder state value =
    if value % 2 = 0 then
        printf "%i \n" (value)
        state + value
    else
        printf "%i \n" value
        state

let initState = 0

List.fold folder initState testList

let folderback value state =
    if value % 2 = 0 then
        printf "%i \n" (value)
        state + value
    else
        printf "%i \n" value
        state

List.foldBack folderback testList initState

List.fold2 (fun acc x y -> (x + y) :: acc) [] [ 1; 2; 3; 4; 5 ] [ 1; 2; 3; 4; 5 ]
List.foldBack2 (fun x y acc -> (x + y) :: acc) [ 1; 2; 3; 4; 5 ] [ 1; 2; 3; 4; 5 ] []
