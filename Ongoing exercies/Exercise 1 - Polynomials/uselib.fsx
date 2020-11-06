#r @"./Polynomials/bin/Debug/netstandard2.0/Polynomials.dll"
open Polynomials.Polynomials

(* Test from Appendix B *)

let p1 = ofList [1; 2];;
//val p1 : Polynomial.Poly = 1 + 2x
let p2 = ofList [3;4;5];;
// val p2 : Poly = 3 + 4x + 5x^2
let p3 = ofList [0;0;0;0;2];;
// val p3 : Poly = 2x^4
let p4 = p1 + p2*p3;;
// val p4 : Poly = 1 + 2x + 6x^4 + 8x^5 + 10x^6
// let p5 = compose p4 p3;;
// val p5 : Poly = 1 + 4x^4 + 96x^16 + 256x^20 + 640x^24
// let p6 = derivative p5;;
// val p6 : Poly = 16x^3 + 1536x^15 + 5120x^19 + 15360x^23
// let d = max (deg p4) (deg p6);;
let d : Degree = Fin 23

(* My own tests *)

p1.Print

add p1 p2

mulC 2 p2

sub p1 p2

mulX [ 2; 0; 0; 1 ]

mul [ 2; 3; 0; 1 ] [ 1; 2; 3 ]

eval 2 [ 2; 3; 0; 1 ]

isLegal [ 2; 3; 0; 1; 0 ]
isLegal [ 2; 3; 0; 1 ]

sum [ 2; 3; 0; 1; 0; 0; 0 ]

prune [ 2; 3; 0; 1; 0 ]
prune [ 2; 3; 0; 1; 0; 0; 0 ]

toString (P([ 2; 3; 0; 1; 0 ]))
toString (P([ 2; -3; 0; 1; 0 ]))
toString (P([ -2; 3; 0; 1; 0 ]))
toString (P([ 2; -3; 0; -1; 3 ]))
toString (P([ -1; -1; -1; -1 ]))
toString (P([ 1; 1; 1; 1 ]))
toString (P([ 0; 1; 1; 1 ])) // denne case kunne forbedres således at 0+ ikke optræder først

toString (P([ 1; 1; 1; 1 ]))
toString (P(derivative [ 1; 1; 1; 1 ]))

deg [ 2; 0; 0; 1 ]
deg [ 0 ]
deg [ 5 ]
deg [ -5 ]
deg []

addD (Fin(7)) (Fin(8))

eval 2 p1