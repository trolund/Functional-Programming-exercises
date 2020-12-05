open System

type Container =
    | Tank of float * float * float // (length, width, height)
    | Ball of float // radius
    | Cylinder of float * float // radius, height

let t = Tank(2.0, 2.0, 2.0)
let b = Ball(2.0)
let b2 = Ball(-2.0)

let isWF =
    function
    | Tank (l, w, h) -> l > 0.0 && w > 0.0 && h > 0.0
    | Ball (r) -> r > 0.0
    | _ -> false


let volume =
    function
    | Tank (l, w, h) -> l * w * h
    | Ball (r) -> 3.0 / 4.0 * Math.PI * (r ** 3.0)
    | _ -> failwith "Not a container"

let volume2 =
    function
    | Tank (l, w, h) -> l * w * h
    | Ball (r) -> 3.0 / 4.0 * Math.PI * (r ** 3.0)
    | Cylinder (r, h) -> Math.PI * (r ** 2.0) * h
    | _ -> failwith "Not a container"

let isWF2 =
    function
    | Tank (l, w, h) -> l > 0.0 && w > 0.0 && h > 0.0
    | Ball (r) -> r > 0.0
    | Cylinder (r, h) -> r > 0.0 && h > 0.0
    | _ -> false

isWF2 t
isWF2 b
isWF2 b2

type Name = string
type Contents = string
type Storage = Map<Name, Contents * Container>


let s =
    Map.ofList [ ("tank1", ("oil", Tank(2.0, 2.0, 2.0)))
                 ("ball1", ("water", Ball(2.0))) ]


let find n stg =
    let item = Map.tryFind n stg

    match item with
    | Some (contents, container) ->
        let volume = volume container
        (contents, volume)
    | None -> failwith "Name does not exist"

find "tank1" s
