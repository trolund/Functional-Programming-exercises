# Cheat sheet for F#

`<>` er det samme som `!=` 

## Types

### Delcare type

```fs
type Name = string

type Flow = int

type River = R of Name * Flow * Tributaries

and Tributaries = River list
```

### Construct obj of type

```fs
// River
let riv = R("R3", 8, [])
```

### Deconstruct type
```fs
// n:Name, f:Flow, t:Tributaries
let (R(n, f, t)) = riv
```

## Options
A option is either None or Some.

You can return None by simple typing `None`. When returning Some, you will write `Some(<obj>)`, where `obj` is the object to return.


## Time complexity

### Append 

@ = Concatenates two lists.
:: = Creates a list. The element on the left side is prepended to the list on the right side.

```fs
[a,a,a] @ ys
```
First argument as three elements, then it takes three steps

[] @ [] @ [] is proportional to the number of elements added

The running time of append is linear on the first argument.

# Functions

Så fremt `Function` keywordet bliver brugt og functionen tager mod mere end et argument vil disse skulle gangives som en `tuple`.

## Recusrive

### Tail recusriver
Mest effektive er akkumerlerede parameter. 

Dette kan man ikke altid, så kan man bruge continoues based, det står på side

## Give an evaluation
See page 211


## Recursion

## Accumulating parameter

## Continuation based

These examples show:

1.  The version using an accumulating parameter is much faster (about five times) than thatusing continuations.

2.  The version using continuations can handle about 30% longer lists.

The run-time disadvantage of a continuation-based declaration is even more clear with aniterative function likefactAwhere no data structure is required to be built in the heap,when compared tobigListC. See Exercise 9.6


## `id`


## Sequences Expressions

En sekvens defineres:

```fs
seq { expr }
```

Eksempler:

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


## yield and yield! 

yield = bruges til at tilføje enkelt element.
yield! = bruges til at tilføje en liste a elementer.