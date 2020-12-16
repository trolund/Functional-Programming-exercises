// 1.

(*

    [0; 1; -2; 3; -4; 5; -6; 7; -8; 9]
*)

let mySeq =
    Seq.initInfinite (fun i -> if i % 2 = 0 then -i else i)

Seq.take 10 mySeq

// 2.

let finSeq n m =
    seq {
        for m' in 0 .. m do
            yield n + 2 * m'
    }

finSeq 3 3
// 3.

