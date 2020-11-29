type Appliance = string
type Usage = Appliance * int

let ad1: Usage = ("washing machine", 2)
let ad2: Usage = ("coffee machine", 1)
let ad3: Usage = ("dishwasher", 2)
let ats: Usage list = [ ad1; ad2; ad3; ad1; ad2 ]

let ats2 =
    [ ad1
      ad2
      ad3
      ad1
      ad2
      ("Unknown", -10) ]



let rec inv =
    function
    | [] -> true
    | head :: tail ->
        let (app, time) = head
        time >= 0 && inv tail

inv ats
inv ats2

let durationOf (app: Appliance) (uts: Usage list) =
    let (_, resTime) =
        List.reduce (fun acc elem ->
            let (curApp, time) = elem
            let (app, accTime) = acc
            if curApp = app then (app, accTime + time) else (app, accTime)) uts

    resTime

durationOf "washing machine" ats

let wellFormed ats =
    let sum =
        List.map (fun elem ->
            let (_, time) = elem
            time) ats

    List.reduce (fun acc x -> x + acc) sum
    <= 24
    && inv ats

wellFormed ats


let delete (a, ats) = List.filter (fun x -> a <> x) ats

delete (ad2, ats)

let trf =
    Map.ofList [ "washing machine", 100
                 "coffee machine", 200
                 "dishwasher", 50 ]

let trf2 =
    Map.ofList [ "washing machine", 100
                 "coffee machine", 200 ]


let rec isDefined ats trf =
    match ats with
    | [] -> true
    | head :: tail ->
        let (name, _) = head

        let check n =
            match Map.tryFind n trf with
            | Some (_) -> true
            | None -> false

        check name && isDefined tail trf


isDefined ats trf2


let priceOf ats trf =

    let rec sum ats acc =
        match ats with
        | [] -> acc
        | head :: tail ->
            let (name, hours) = head

            let check n =
                match Map.tryFind n trf with
                | Some (price) -> price
                | None -> failwith "En pris var ikke defineret"

            sum tail (check name + acc)

    sum ats 0

priceOf ats trf
