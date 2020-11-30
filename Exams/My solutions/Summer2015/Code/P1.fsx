let repeat s t =

    let rec loop acc n =
        match n with
        | 0 -> acc
        | _ -> s + loop acc (n - 1)

    loop "" t

repeat "ab" 4
