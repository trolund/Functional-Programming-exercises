type T<'a> =
    | L
    | N of T<'a> * 'a * T<'a>

let rec f g t1 t2 =
    match (t1, t2) with
    | (L, L) -> L
    | (N (ta1, va, ta2), N (tb1, vb, tb2)) -> N(f g ta1 tb1, g (va, vb), f g ta2 tb2)

let rec h t =
    match t with
    | L -> L
    | N (t1, v, t2) -> N(h t2, v, h t1)

let rec g =
    function
    | (_, L) -> None
    | (p, N (t1, a, t2)) when p a -> Some(t1, t2)
    | (p, N (t1, a, t2)) ->
        match g (p, t1) with
        | None -> g (p, t2)
        | res -> res

let t = N(N(L, 1, N(N(L, 2, L), 1, L)), 3, L)

let tbool = N(L, [ true ], L)


let count a t =

    let rec aux count =
        function
        | L -> count
        | N (t1, v, t2) when v = a -> 1 + aux count t1 + aux count t2
        | N (t1, _, t2) -> aux count t1 + aux count t2

    aux 0 t

count 1 t


let rec replace a b =
    function
    | L -> L
    | N (t1, v, t2) when v = a -> N(replace a b t1, b, replace a b t2)
    | N (t1, v, t2) -> N(replace a b t1, v, replace a b t2)

replace 1 0 t
