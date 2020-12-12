(* Problem 1 (30%) *)

type Name = string
type Phone = int
type Level = int
type Description = Phone * Level

type Register = (Name * Description) list

// 1.

let reg =
    [ ("Joe", (10101010, 4))
      ("Sam", (12121212, 7))
      ("Jane", (13131313, 1)) ]

// 2.

let rec getPhone n =
    function
    | [] -> failwith "No one has that name in reg"
    | (name, d) :: _ when n = name ->
        let (nr, level) = d
        nr
    | _ :: tail -> getPhone n tail

getPhone "Joe" reg
getPhone "Sam" reg
getPhone "Jane" reg

// 3.

let rec delete n =
    function
    | [] -> []
    | (name, d) :: tail when n = name -> delete n tail
    | head :: tail -> head :: delete n tail

delete "Joe" reg

delete "Jane" reg

// 4.

let rec getCandidates l =
    function
    | [] -> []
    | (name, (p, tl)) :: tail when (abs l - tl) < 3 -> (name, p) :: getCandidates l tail
    | _ :: tail -> getCandidates l tail

getCandidates 5 reg
