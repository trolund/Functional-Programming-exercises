(*
    9.8 Develop a version of the counting function for binary trees
    countA: int -> BinTree<’a> -> int
    that makes use of an accumulating parameter. Observe that this function is not tail recursive.

    look at page 133
*)

type BinTree<'a> = | Leaf
                   | Node of BinTree<'a> * 'a * BinTree<'a>;;