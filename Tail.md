Start with a simple task, like mapping items from 'a to 'b in a list. We want to write a function which has the signature

    val map: ('a -> 'b) -> 'a list -> 'b list

Where

```fsharp
map (fun x -> x * 2) [1;2;3;4;5] == [2;4;6;8;10]
```

#### Start with non-tail recursive version:

```fsharp
let rec map f = function
    | [] -> []
    | x::xs -> f x::map f xs
```

This isn't tail recursive because function still has work to do after making the recursive call. :: is syntactic sugar for List.Cons(f x, map f xs).

The function's non-recursive nature might be a little more obvious if I re-wrote the last line as | x::xs -> let temp = map f xs; f x::temp -- obviously its doing work after the recursive call.

Use an accumulator variable to make it tail recursive:

```fsharp
let map f l =
    let rec loop acc = function
        | [] -> List.rev acc
        | x::xs -> loop (f x::acc) xs
    loop [] l
```

Here's we're building up a new list in a variable acc. Since the list gets built up in reverse, we need to reverse the output list before giving it back to the user.

If you're in for a little mind warp, you can use continuation passing to write the code more succinctly:

```fsharp
let map f l =
    let rec loop cont = function
        | [] -> cont []
        | x::xs -> loop ( fun acc -> cont (f x::acc) ) xs
    loop id l
```

Since the call to loop and cont are the last functions called with no additional work, they're tail-recursive.

This works because the continuation cont is captured by a new continuation, which in turn is captured by another, resulting in a sort of tree-like data structure as follows:

    (fun acc -> (f 1)::acc)
        ((fun acc -> (f 2)::acc)
            ((fun acc -> (f 3)::acc)
                ((fun acc -> (f 4)::acc)
                    ((fun acc -> (f 5)::acc)
                        (id [])))))
                        
which builds up a list in-order without requiring you to reverse it.

For what its worth, start writing functions in non-tail recursive way, they're easier to read and work with.

If you have a big list to go through, use an accumulator variable.

If you can't find a way to use an accumulator in a convenient way and you don't have any other options at your disposal, use continuations. I personally consider non-trivial, heavy use of continuations hard to read.