let rec f =
    function
    | 0 -> [ 0 ]
    | i when i > 0 -> i :: g (i - 1)
    | _ -> failwith "Negative argument"

and g =
    function
    | 0 -> []
    | n -> f (n - 1)

f 5

let h s k =
    seq {
        for a in s do
            yield k a
    }

h (seq [ 1; 2; 3; 4 ]) (fun i -> i + 10)
