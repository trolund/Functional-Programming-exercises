// Week 2

// 2.1

let divisible n = (n % 2 = 0 || n % 3 = 0) && n % 5 <> 0

divisible (24)
divisible (27)
divisible (29)
divisible (30)

// 2.2

let rec pow (s, n) =
    match (s, n) with
    | (_, n) when n = 0 -> ""
    | (s, _) -> s + pow (s, n - 1)

let rec pow2 (s, n) =
    match n with
    | 0 -> ""
    | n -> s + pow (s, n - 1)

pow ("hej", 3)
pow2 ("hej", 3)
