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




buildString 4 "XO" ""




aux m n ""

let viz m n =

    let rec buildString m (s: string) acc =
        match m with
        | 0 -> acc
        | _ -> buildString (m - 1) s (s + acc)

    let rec aux n' acc =
        match n' with
        | 0 -> acc
        | nn when nn % 2 = 0 -> aux (nn - 1) (acc + (buildString m "OX" "") + "\n")
        | nn -> aux (nn - 1) (acc + (buildString m "XO" "") + "\n")

    aux n ""

printfn "%s" (viz 4 5)



let repeatTail s t =

    let rec loop acc n =
        match n with
        | 0 -> acc
        | _ -> loop (s + acc) (n - 1)

    loop "" t

repeatTail "hej" 3


let repeatCon s t =

    let rec loop f n =
        match n with
        | 0 -> f ""
        | _ -> loop (fun acc -> f (s + acc)) (n - 1)

    loop id t

repeatCon "hej" 3
