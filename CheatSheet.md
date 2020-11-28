# Cheat sheet for F#


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
