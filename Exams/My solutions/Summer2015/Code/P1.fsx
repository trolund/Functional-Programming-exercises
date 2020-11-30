let repeat s t =

    let rec loop acc n =
        match n with
        | 0 -> acc
        | _ -> s + loop acc (n - 1)

    loop "" t

repeat "ab" 4


let f s1 s2 t =

    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ -> if n % 2 = 0 then loop (n - 1) (s1 + "\n" + acc) else loop (n - 1) (s2 + "\n" + acc)

    loop t ""

f "XO" "OX" 3
