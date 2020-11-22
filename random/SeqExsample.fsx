let sift a sq = Seq.filter (fun n -> n % a <> 0) sq;;

let rec sieve sq =
    seq { let p = Seq.head sq 
          yield p
          yield! sieve(sift p (Seq.tail sq)) }

let primes = sieve(Seq.initInfinite (fun n -> n+2))
primes

let nthPrime n = Seq.item n primes

nthPrime 102