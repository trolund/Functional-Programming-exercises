(* Question 1 (25 %) *)

type Item = { Id: int; Name: string; Price: float }
type Register = Item list

// 1.

let i1 = { Id = 1; Name = "Milk"; Price = 8.75 }

let i2 =
    { Id = 2
      Name = "Juice"
      Price = 16.25 }

let i3 =
    { Id = 3
      Name = "Rye Bread"
      Price = 25.00 }

let i4 =
    { Id = 4
      Name = "White Bread"
      Price = 18.50 }

let reg: Register = [ i1; i2; i3; i4 ]

// 2.

let getItemById i r =
    match List.tryFind (fun x -> x.Id = i) r with
    | Some (v) -> v
    | None ->
        failwith
            ("item with id "
             + string (i)
             + " kunne ikke findes")

// getItemById 10 reg

getItemById 1 reg

// 3.

let nextId r =
    1 + (List.map (fun x -> x.Id) r |> List.max)

nextId reg

// 4.

let addItem n p r =
    { Id = nextId r; Name = n; Price = p } :: r

addItem "skum" 100.00 reg

// 5.

let deleteItemById id r = List.filter (fun x -> x.Id <> id) r

deleteItemById 1 reg

// 6.

let rec uniqueRegister =
    function
    | [] -> true
    | x :: xs ->
        List.forall (fun y -> y.Id <> x.Id) xs
        && uniqueRegister xs

uniqueRegister reg

let reg2: Register = [ i1; i2; i3; i4; i4 ]

uniqueRegister reg2

// 7.

let itemsInPriceRange max min r =
    List.filter (fun x -> x.Price <= max && x.Price >= min) r

itemsInPriceRange 17.0 8.0 reg
