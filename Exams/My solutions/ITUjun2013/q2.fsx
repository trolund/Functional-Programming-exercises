let rec f n m =
    if m = 0 then n else n * f (n + 1) (m - 1)

(*
1.

n angiver "start talet" og m angiver antalet af gange der skal ganges

derfor vil f 6 3 eksemplevis være 6*7*8. f 2 4 vil derved være  2*3*4*5

*)

// 2.

let f2 n m =
    let rec aux n m acc =
        if m = 0 then n else aux (n + 1) (m - 1) (n * acc)

    aux n m 1

f2 6 3
f 6 3
