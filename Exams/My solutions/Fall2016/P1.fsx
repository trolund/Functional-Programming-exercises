(* Problem 1 (20%) *)

type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list

let sb =
    [ ("Joe", "June Fishing", 35)
      ("Peter", "May Fishing", 30)
      ("Joe", "May Fishing", 28)
      ("Paul", "June Fishing", 28) ]

let sb2 =
    [ ("Joe", "June Fishing", 20)
      ("Peter", "May Fishing", 30)
      ("Joe", "May Fishing", 28)
      ("Paul", "June Fishing", 28) ]

let sb3 =
    [ ("Joe", "June Fishing", -100)
      ("Peter", "May Fishing", 30)
      ("Joe", "May Fishing", 28)
      ("Paul", "June Fishing", 28) ]

// 1.

let inv s =

    let rec aux lastP =
        function
        | [] -> true
        | (_, _, points) :: tail -> if points <= lastP && points >= 0 then aux points tail else false

    aux 99999 s

inv sb
inv sb2

inv sb
// 2.

// let rec insert s sb =
//     let rec aux acc lastP =
//         function
//         | [] -> true
//         | head :: tail -> let (_, _, p) = head
//                           if p > lastP then acc :: s :: tail
//                           else aux (head :: acc) p
//     aux [] (List.rev sb)



let get (n, sb) =

    let rec loop acc =
        function
        | [] -> acc
        | (na, e, p) :: tail when n = na -> loop ((e, p) :: acc) tail
        | _ :: tail -> loop acc tail

    loop [] sb

get ("Joe", sb)

let rec top n sb =

    let rec aux n acc sb =
        match n, sb with
        | k, _ when k < 0 -> None
        | 0, _ -> Some(List.rev acc)
        | k, head :: tail -> aux (k - 1) (head :: acc) tail

    aux n [] sb

top 2 sb
