# Problem 3 from the exam fall 2015

```fsharp
let rec g1 p = 
    function
    | x::xs when p x -> x :: g1 p xs
    | _ -> []

let rec g2 f h n x =
    match n with
    | _ when n<0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n-1) (f x);;
```

1. Give the (most general) types of g1 and g2 and describe what each of these two functions
computes. Your description for each function should focus on what it computes, rather
than on individual computation steps.

type g1: (a' -> bool) -> b' list -> b' list 
formål: filtere en liste således at kun elementer som matcher funktionen p angiver skal være i listen.

type g2: 

2. The function g1 is not tail recursive.
    • Make a tail-recursive variant of g1 using an accumulating parameter.
    • Make a continuation-based tail-recursive variant of g1.