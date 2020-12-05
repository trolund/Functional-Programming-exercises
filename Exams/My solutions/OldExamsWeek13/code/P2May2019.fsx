type Part = string
type Task = string
type Cost = int (* can be assumed to be positive *)
type Duration = int (* can be assumed to be positive *)
type PartReg = Map<Part, Cost>
type TaskReg = Map<Task, Duration * Cost>


(* Part and task registers for balance bikes *)

let preg1 =
    Map.ofList [ ("wheel", 50)
                 ("saddle", 10)
                 ("handlebars", 75)
                 ("frame", 100)
                 ("screw bolt", 5)
                 ("nut", 3) ]

let treg1 =
    Map.ofList [ ("addWheels", (10, 2))
                 ("addSaddle", (5, 2))
                 ("addHandlebars", (6, 1)) ]

type WorkStation = Task * (Part * int) list
type AssemblyLine = WorkStation list


let ws1 =
    ("addWheels",
     [ ("wheel", 2)
       ("frame", 1)
       ("screw bolt", 2)
       ("nut", 2) ])

let ws2 =
    ("addSaddle",
     [ ("saddle", 1)
       ("screw bolt", 1)
       ("nut", 1) ])

let ws3 =
    ("addHandlebars",
     [ ("handlebars", 1)
       ("screw bolt", 1)
       ("nut", 1) ])

let ws4 =
    ("addHandlebars",
     [ ("handlebars", 1)
       ("screw bolt", 1)
       ("nut", 1)
       ("Saw", 1) ])

let al1 = [ ws1; ws2; ws3 ]


let rec wellDefWS (preg: PartReg) (treg: TaskReg) (ws: WorkStation) =
    let (name, parts) = ws
    let rule1 = Map.containsKey name treg

    let rule23 =
        List.forall (fun (part, _) -> Map.containsKey part preg) parts

    rule1 && rule23

wellDefWS preg1 treg1 ws1 // TRUE
wellDefWS preg1 treg1 ws2 // TRUE
wellDefWS preg1 treg1 ws3 // TRUE
wellDefWS preg1 treg1 ws4 // FALSE

let wellDefAL preg treg al =
    List.forall (fun ws -> wellDefWS preg treg ws) al


let longestDuration (al: AssemblyLine, treg: TaskReg) =
    List.fold (fun max (name, preg) ->
        let (dur, cost) = Map.find name treg
        if dur > max then dur else max) 0 al

longestDuration (al1, treg1)


let partCostAL (preg: PartReg) al =

    let rec partlist acc =
        function
        | [] -> acc
        | (name, amount) :: tail ->
            let cost = Map.find name preg
            partlist (acc + (cost * amount)) tail

    let rec loop acc =
        function
        | [] -> acc
        | (name, plist) :: tail -> loop (acc + partlist 0 plist) tail

    loop 0 al

partCostAL preg1 al1


let prodDurCost (treg: TaskReg) (al: AssemblyLine) =

    List.fold (fun acc ws ->
        let (name, partlist) = ws
        let (dur, cost) = Map.find name treg
        let (da, ca) = acc
        (da + dur, ca + cost)) (0, 0) al

prodDurCost treg1 al1

// alternativ

let prodDurCostA (treg: TaskReg) (al: AssemblyLine) =

    List.fold (fun (da, ca) (name, _) ->
        let (dur, cost) = Map.find name treg
        (da + dur, ca + cost)) (0, 0) al


let toStock al =
    let rec aux acc =
        function
        | [] -> acc
        | (name, partlist) :: tail -> aux (acc @ partlist) tail

    Map.ofList (aux [] al)

toStock al1
