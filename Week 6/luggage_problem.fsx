type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list
type ArrivalCatalogue = (Airport * Lid list) list


let cat: LuggageCatalogue =
    [ ("DL 016-914",
       [ ("DL 189", "ATL")
         ("DL 124", "BRU")
         ("SN 733", "CPH") ])
      ("SK 222-142",
       [ ("SK 208", "ATL")
         ("DL 124", "BRU")
         ("SK 122", "JFK") ]) ]

let rec findRoute (id: Lid, lCat: LuggageCatalogue) =
    match lCat with
    | [] -> failwith ("No route found with id: " + id)
    | (i, r) :: xs -> if i = id then r else findRoute (id, xs)

let result = findRoute ("DL 016-914", cat)


let rec inRoute (fId: Flight, route: Route) =
    match route with
    | [] -> false
    | (i, a) :: xs -> if i = fId then true else inRoute (fId, xs)

inRoute ("SK 122", result)
inRoute ("DL 189", result)

let rec withFlight f lc =
    match lc with
    | [] -> []
    | (i, r) :: xs -> if inRoute (f, r) then i :: withFlight f xs else withFlight f xs

withFlight "DL 124" cat


let aCat: ArrivalCatalogue =
    [ ("ATL", [ "DL 016-914"; "SK 222-142" ])
      ("BRU", [ "DL 016-914"; "SK 222-142" ])
      ("JFK", [ "SK 222-142" ])
      ("CPH", [ "DL 016-914" ]) ]

let extend (id:Lid,r:Route,aCat:ArrivalCatalogue) = 
    let rec isAirportInRoute (airp,route)  = match route with
                                             | (_,a)::xs -> if airp = a then true else isAirportInRoute (airp,xs)
                                             | [] -> false
    let rec t apCat = 
            match apCat with
            | [] -> []
            | (a,l) -> if isAirportInRoute (a,r) then id::l else // Ad new airpot

    