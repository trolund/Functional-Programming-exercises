// HR 5.3 - Solve Exercise 4.12 using List.fold or List.foldBack.

(* Declare a function sum(p, xs ) where p is a predicate of type int -> bool and xs is a list of integers. 
The value of sum(p,xs) is the sum of the elements in xs satisfying the predicate p. 
Test the function on different predicates (e.g., p(x) = x > 0). *)

let p1 v = v % 2 = 0
let p2 v = (v % 2) <> 0
let p3 v = v > 0
let p4 v = v > 2

let rec sum p xs = List.fold (fun x acc -> if p x then x + acc else acc ) 0 xs

sum p3 [1;2;3;4]
sum p1 [1;2;3;4]
sum p2 [1;2;3;4]
sum p4 [1;2;3;4]