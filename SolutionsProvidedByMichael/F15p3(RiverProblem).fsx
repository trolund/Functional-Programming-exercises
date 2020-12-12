// Michael R. Hansen 08-11 2020


// Problem 3 35 %

type Name       = string
type Flow       = int
type River      = R of Name * Flow * Tributaries
and Tributaries = River list


// 1
let r1 = R("R1", 10, [])
let r3 = R("R2", 8, [])
let r4 = R("R4", 2, [])
let r2 = R("R2", 15, [r4])
let riv = R("R", 5, [r1;r2;r3]);;

(* 
There are many solutions to the below programming problems. Since the types 
River and Tributaties are declared in mutual recursion, one approach is using 
mutually recursive declarations of functions. 

*)

// 2
// contains: Name -> River -> bool
let rec contains n = 
   function
   | R(n',_,_) when n=n' -> true
   | R(_,_,ts)           -> List.exists (contains n) ts;;

// alternative solution
// contains1: Name -> River -> bool
// containsTs: Name -> Tributaries -> bool
let rec contains1 n = 
   function
   | R(n',_,_) when n=n' -> true
   | R(_,_,ts)           -> containsTs n ts
and containsTs n = 
   function
   | []     -> false
   | r::ts  -> contains1 n r || containsTs n ts;;

// 3                          
// allNames: River -> Name list
let rec allNames (R(n,fl,ts)) = n::List.collect allNames ts     

// 4
// totalFlow: River -> int
let rec totalFlow (R(_,f,ts)) = List.fold (fun s r -> s + totalFlow r) f ts;;

// alternative solution
// totalFlow1: River -> int
// totalFlowTs: Tributaries -> int
let rec totalFlow1 (R(_,f,ts)) = f + totalFlowTs ts
and totalFlowTs = function
                  | []    -> 0
                  | r::ts -> totalFlow1 r + totalFlowTs ts;;

// 5
// maxFlow: Name*Flow -> Name*Flow -> Name*Flow
let maxFlow (n,fl) (n',fl') = if fl>fl' then (n,fl) else (n',fl') 
                                    
// mainSource: River -> Name*Flow
// mainSourceTs: (Name*Flow) -> Tributaries -> Name*Flow
let rec mainSource (R(n,fl,ts)) = mainSourceTs (n,fl) ts
and mainSourceTs res = 
   function
   | []    -> res
   | r::ts -> mainSourceTs (maxFlow res (mainSource r)) ts       


// 6
// tryInsertTributary: Name -> River -> River -> River option
// tryInsertTributaryInList: Name -> River -> Tributaties -> Tributaries option
let rec tryInsertTributary n t = 
   function
   | R(n',fl,ts) when n=n' -> Some(R(n',fl,t::ts))
   | R(n',fl,ts)           -> match tryInsertTributaryInList n t ts with
                              | None     -> None
                              | Some ts' -> Some(R(n',fl,ts'))
and tryInsertTributaryInList n t =
   function 
   | []    -> None
   | r::ts -> match tryInsertTributary n t r with 
              | None    -> match tryInsertTributaryInList n t ts with
                           | None     -> None
                           | Some ts' -> Some(r::ts')
              | Some r' -> Some(r'::ts);;
              
// 7 
// A river may split (river bifurcaion), that is, it divides into two streams. 
// The streams (from a river that splits), may actually merge again.
// The tree model does not capture these cases.  
// A model based on directed acyclic graphs would be adequate in these cases.
