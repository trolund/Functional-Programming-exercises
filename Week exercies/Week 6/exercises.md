# Task 1
## 1

Du kan tydeligt se at function f tager i mod i input værdi 'a og returnere en liste. Denne liste
kan være arbitær, så den må have typen 'b. Collect kører over hvert element og fodre det til f, så
vi tager med en 'a liste og retunerer en 'b liste, da vi (at)'er outputtet fra f til resten af 
elementerne. 


## 2

```fsharp
let rec collect f = function
    | []    -> []
    | x::xs -> f x @ collect f xs;;
```
collect (fun (a,b) -> [a..b]) [(1,3); (4,7); (8,8)];;

[1; 2; 3; 4; 5; 6; 7; 8]

Step 1:
``` fsharp
   collect (fun (a,b) -> [a..b]) [(1,3); (4,7); (8,8)];;
-> [1..3] @ collect (fun (a,b) -> [a..b]) [(4,7); (8,8)]
-> [1,2,3] @ collect (fun (a,b) -> [a..b]) [(4,7); (8,8)]
-> [1,2,3] @ [4..7] @ collect (fun (a,b) -> [a..b]) [(8,8)]
-> [1,2,3] @ [4,5,6,7] @ collect (fun (a,b) -> [a..b]) [(8,8)]
-> [1,2,3] @ [4,5,6,7] @ [8..8] @ collect (fun (a,b) -> [a..b]) []
-> [1,2,3] @ [4,5,6,7] @ [8] @ collect (fun (a,b) -> [a..b]) []
-> [1,2,3] @ [4,5,6,7] @ [8] @ []
-> [1,2,3,4,5,6,7,8]
```

## 3

```fsharp
collect (fun (a,b) -> [a..b]) [(1,3); (4,7); (8,8)] 
// ( (int * int) -> int list) -> (int * int) list -> int list
```

# Task 2

