// Draft of a solution to Problem 1, Exam fall 2013
//                     Michael R. Hansen 12/10/2020

type Multiset<'a when 'a : equality> = ('a * int) list;;

// Q 1.1
// There are many ways to solve the problem
// Two are given

// inv: Multiset<'a> -> bool
let rec inv = 
   function
   | [] -> true
   | (e,k)::m -> k>0 && 
                 List.forall (fun (e',_) -> e <> e') m && 
                 inv m;;

// inv': Set<'a> -> Multiset<'a> -> bool
let rec inv' es = 
   function 
   | []                                       -> true
   | (e,k)::_ when k<=0 || Set.contains e es  -> false
   | (e,_)::ms                                -> inv' (Set.add e es) ms;;

// inv1: Multiset<'a> -> bool
let inv1 ms = inv' Set.empty ms                   

let ms1 = [("b",3); ("a",5); ("d",1)];;

// Q 1.2
// For insert e k m you can assume that m satiefies the multiset invariant
// but you must consider the situation where k<=0. 
//
// The presented solution raises an exception when k<=0. This is reasonable 
// because the function is named insert and there also is a delete function in the 
// problem. 

// Is is certainly consistent and a fine solution to allow that k<=0 and then 
// delete |k| occurrences from m when k<=0.

// Advice: it is a good idea to write about
// such choices and considarations in the solution.

// insert: 'a -> int -> Multiset<'a> -> Multiset<'a> 
let rec insert e k m = 
   if k <= 0 then failwith "multiset: argument error"
   else match m with 
        | []                    -> [(e,k)]
        | (e',k')::m' when e=e' -> (e,k+k')::m'
        | pair::m'              -> pair::insert e k m';;

// Q1.3
// numberOf: 'a -> Multiset<'a> -> int 
let rec numberOf e = function
                     | []                  -> 0
                     | (e',k)::m when e=e' -> k
                     | _::m'               -> numberOf e m';;

// Q1.4
// delete: 'a -> Multiset<'a> -> Multiset<'a> 
let rec delete e = function
                   | [] -> []
                   | (e',1)::m when e=e' -> m
                   | (e',k)::m when e=e' -> (e,k-1)::m
                   | pair::m             -> pair::delete e m;;

// Q1.5
// union: Multiset<'a> * Multiset<'a> -> Multiset<'a>
// A solution using foldBack
// A recursive function based on repeated insertion is also fine

let union(m1, m2) = List.foldBack (fun (e,k) m -> insert e k m)  m1 m2;;
 
let mu =  union ([("b",3); ("a",5); ("d",1)], [("a",3); ("b",4); ("c",2)]);; 

// Q1.6
//

type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;

// inv2: MultisetMap<'a> -> bool 
let inv2 m = Map.forall (fun _ n -> n>0) m;;    

// insert2: 'a -> int -> MultisetMap<'a> -> MultisetMap<'a>
let insert2 e k m = 
    if k<=0 then failwith "insert2: argument error"
    else match Map.tryFind e m with  
         | None    -> Map.add e k m
         | Some k' -> Map.add e (k+k') m;;

let union2(m1, m2) = Map.foldBack insert2 m1 m2;;

union2 (Map.ofList[("b",3); ("a",5); ("d",1)], Map.ofList [("a",3); ("b",4); ("c",2)]);;

                                                                           