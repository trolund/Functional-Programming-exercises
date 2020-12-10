type Multiset<'a when 'a: equality> = ('a * int) list

let data: Multiset<string> = [ ("b", 3); ("a", 5); ("d", 1) ]

let rec inv ms =
    match ms with
    | [] -> true
    | (key, _) :: tail ->
        (List.fold (fun acc (k, _) -> if k = key then acc + 1 else acc) 0 ms) = 1
        && inv tail

inv data


let rec insert e n ms =
    match ms with
    | [] -> []
    | head :: tail ->
        let (k, v) = head
        if k = e then (k, v + n) :: insert e n tail else head :: insert e n tail

insert "b" 5 data
insert "a" 5 data
insert "d" 5 data


let rec numberOf e =
    function
    | [] -> failwith ("Der var ingen " + e + " i multitsættet")
    | (k, v) :: _ when k = e -> v
    | _ :: tail -> numberOf e tail

numberOf "a" data


let rec delete e ms =
    match ms with
    | [] -> []
    | head :: tail ->
        let (k, v) = head
        if k = e then delete e tail else head :: delete e tail

delete "a" data

let rec union ms1 ms2 =
    match ms1, ms2 with
    | [], [] -> []
    | ms1', [] -> ms1' @ ms2
    | [], ms2' -> ms2'
    | (k, v) :: tail, ms2' -> union tail (insert k v ms2')

union data data

type MultisetMap<'a when 'a: comparison> = Map<'a, int>

let mapData =
    Map.ofList [ ("a", 5)
                 ("b", 2)
                 ("d", 4) ]

let mapData2 =
    Map.ofList [ ("a", 5)
                 ("b", 6)
                 ("c", 7) ]


let insertMap e n ms =
    match Map.tryFind e ms with
    | Some (v) -> Map.remove e ms |> Map.add e (v + n)
    | None -> Map.add e n ms

insertMap "a" 2 mapData

let unionMap ms1 ms2 =
    Map.fold (fun acc key value -> insertMap key value acc) ms1 ms2

unionMap mapData mapData2
