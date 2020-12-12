(* Problem 3 (20%) *)

type 'a Tree =
    | Lf
    | Br of 'a * 'a Tree * 'a Tree


let rec f (n, t) =
    match t with
    | Lf -> Lf
    | Br (a, tl, t2) -> if n > 0 then Br(a, f (n - 1, tl), f (n - 1, t2)) else Lf

let rec g p =
    function
    | Br (a, tl, t2) when p a -> Br(a, g p tl, g p t2)
    | _ -> Lf

let rec h k =
    function
    | Lf -> Lf
    | Br (a, tl, t2) -> Br(k a, h k tl, h k t2)
