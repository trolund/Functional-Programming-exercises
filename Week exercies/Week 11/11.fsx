(* 
    11.1 Make a declaration for the sequence of odd numbers.
*)

// løsning 1
let nat = Seq.initInfinite id |> Seq.filter (fun x -> x % 2 = 1) 
nat

// løsning 2
let res = Seq.initInfinite (fun x -> x*2+1)
res

(*
    11.2 Make a declaration for the sequence of numbers 1, 1, 2, 6, . . . , n!, . . ..
*)

let factorial = 
    let rec fact c s = seq { 
                              yield (s*c)
                              yield! fact (c+1) (s*c)
                            }
    fact 1 1

factorial
Seq.toList (Seq.take 100 factorial)


(*
    11.3 Makeadeclarationforthesequenceofseq[1;1;2;6;...;n!;...],wherethei+1’stelementis
    generated from the i′th element by multiplication with i + 1.
*)

(*
    11.9 Declare a sequence denoting the following enumeration of the integers:

                                0,−1,1,−2,2,−3,3,...
*)






