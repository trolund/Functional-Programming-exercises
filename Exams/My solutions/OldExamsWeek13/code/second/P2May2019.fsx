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
       ("What?", 1) ])

let al1 = [ ws1; ws2; ws3 ]
let al2 = [ ws1; ws2; ws3; ws4 ]

let wellDefWS (preg: PartReg) (treg: TaskReg) (ws: WorkStation) =
    let checkTask (t: Task) = Map.containsKey t treg
    let checkPartreg (p: Part) = Map.containsKey p preg
    let (task, parts) = ws
    List.forall (fun (k, p) -> checkPartreg k && p > 0) parts
    && checkTask task

wellDefWS preg1 treg1 ws3
wellDefWS preg1 treg1 ws4

let wellDefAL (preg: PartReg) (treg: TaskReg) (al: AssemblyLine) =
    List.forall (fun x -> wellDefWS preg treg x) al

wellDefAL preg1 treg1 al1
wellDefAL preg1 treg1 al2


let longestDuration (al: AssemblyLine, treg: TaskReg) =
    List.map (fun (n, p) -> Map.find n treg) al

//List.reduce max
