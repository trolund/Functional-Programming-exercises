# Cheat sheet for F#

## Types

### Delcare type

```fs
type Name = string

type Flow = int // simple

type River = R of Name * Flow * Tributaries // tuple
and Tributaries = River list

type Item = { Id: int; Name: string; Price: float } // record

type MultisetMap<'a when 'a: comparison> = Map<'a, int> // generic type
```


### Construct object of type

```fs
// River (R er kontruktør for River typen)
let riv = R("R3", 8, [])


let i1 = { Id = 1; Name = "Milk"; Price = 8.75 }

let mappen = MultisetMap [ ("a", 2); ("b", 3) ]
```

### Deconstruct type
```fs
// n:Name, f:Flow, t:Tributaries
let (R(n, f, t)) = riv
```

### Accress elements in record
```fs
il.Id // 1
il.Name // Milk
```


## Data structures

### Lists

```fs
let list1 = [0..5] // 0; 1; 2; 3; 4; 5;
let list2 = [0..2..6] // 0; 2; 4; 6;
let list3 = [0;1;2;3;4;5] // 0; 1; 2; 3; 4; 5;
let list1 = 0 :: 1 :: 2 :: 3 :: 4
let lit5 = [1;2] @ 1::2@[1;4] // 1; 2; 1; 2; 1; 4
```

#### Pattern matching

```fs
let pattern1 list1 = match list1 with
    | [] -> ...
    | [x] -> ...
    | x::xs -> ...

let pattern2 list2 = match list2 with
    | [] -> ...
    | (x,y)::xs -> ... // deconstruct tuple

let pattern3 list3 = match list3 with
    | [] -> ...
    | (R(x,y))::xs -> ... // deconstruct object
```

### Maps

#### Pattern matching

Try use the helper methods.

https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching

### Sequences

```fs
(* Sequences *)
let seq1 = seq { 1 .. 10 }

(* ascending order and increment*)
printfn "The Sequence: %A" seq1
let seq2 = seq { 1 .. 5 .. 50 }

(* descending order and decrement*)
printfn "The Sequence: %A" seq2

let seq3 = seq {50 .. -5 .. 0}
printfn "The Sequence: %A" seq3

(* using yield *)
let seq4 = seq { for a in 1 .. 10 do yield a, a*a, a*a*a }
printfn "The Sequence: %A" seq4
```

#### Resultater
The Sequence: seq [1; 2; 3; 4; ...]
The Sequence: seq [1; 6; 11; 16; ...]
The Sequence: seq [50; 45; 40; 35; ...]
The Sequence: seq [(1, 1, 1); (2, 4, 8); (3, 9, 27); (4, 16, 64); ...]

```fs
Seq.initInfinite id // Seq<int>

let multTable n =
    Seq.take 10 (Seq.initInfinite (fun x -> (x + 1) * n)) // Seq<int>

Seq.toList (multTable 3) //

let tableOf m n =
    seq {
        for i in 1 .. m do
            for j in 1 .. n do
                yield (i*j)
                yield! seq { for k in 1 .. m do
                    yield k}
    }

tableOf 10 10
```
* `yield` Tilføjer 1 element til sekvensen
* `yield!` Tilføjer en sekvens til sekvensen.
* `Seq.Delay`: Denne funktion delayer udregningen til den er nødvendig. Får man et stackoverflow ved oprettelse af Seq, kan man bruge `Seq.Delay`.

## Option
A option is either None or Some.

You can return None by simple typing `None`. When returning Some, you will write `Some(<obj>)`, where `obj` is the object to return.

```fs
match Map.tryFind key map with
| None -> // Fandt ikke key i map
| Some v -> // fandt v i map. // V er værdien.


// An example list.
let ids = [ 10; 20; 30 ]

// Find index of element with value 30.
// ... This returns an option int.
let result = List.tryFindIndex (fun y -> y = 30) ids

// Write return value.
printfn "Result = %A" result
printfn "IsNone = %A" result.IsNone
printfn "IsSome = %A" result.IsSome

result.IsSome
// See if there is a value.
if result.IsSome then
    // Get and write the value.
    let num = result.Value
    printfn "Value = %A" num
```

### Append 
```fs
[a,a,a] @ ys
```
First argument as three elements, then it takes three steps

[] @ [] @ [] is proportional to the number of elements added

The running time of append is linear on the first argument.



## Recusrive

### Tail recusriver
Mest effektive er akkumerlerede parameter. 

Dette kan man ikke altid, så kan man bruge continoues based, det står på side


## Give an evaluation
See page 211


## Recursion
Lidt lang: https://www.gresearch.co.uk/article/advanced-recursion-techniques-in-f/

###### Declaration of a tail-recursive variant


```fsharp
let g1tail p l = 
    let rec aux acc = function
            | x::xs when p x -> aux (x :: acc) xs
            | _ -> List.rev acc

    aux [] l
```

##### 4. Provide a declaration of a continuation-based, tail-recursive variant off.

```fsharp
let g1con p l =
    let rec loop cont =
        function
        | x :: xs when p x -> loop (fun acc -> cont (x :: acc)) xs
        | _ -> cont []

    loop id l
```

    (fun acc -> (f 1)::acc)
        ((fun acc -> (f 2)::acc)
            ((fun acc -> (f 3)::acc)
                ((fun acc -> (f 4)::acc)
                    ((fun acc -> (f 5)::acc)
                        (id [])))))

###### Hvis datatrukturen minder om et træ / der er flere "veje" at gå

```fsharp
let rec fK t k = match t with
                 | A a         -> k([a],[])
                 | B b         -> k([], [b])
                 | Node(t1,t2) -> fK t1 (fun (xs1,ys1) -> fK t2 (fun (xs2,ys2) -> k(xs1@xs2,ys1@ys2)));;
```


## Accumulating parameter

Remember to maybe revert the list, because the list is build differently. Or use `acc @ [i]`

## Continuation based

These examples show:

1.  The version using an accumulating parameter is much faster (about five times) than using continuations.

2.  The version using continuations can handle about 30% longer lists.

The run-time disadvantage of a continuation-based declaration is even more clear with aniterative function like `factA` where no data structure is required to be built in the heap,when compared tobigListC. See Exercise 9.6

## Difference


## Write about iterative and tail recursive functions