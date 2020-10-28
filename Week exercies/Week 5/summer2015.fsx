// Problem 4 from the exam summer 2015

type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS

type CourseBase = Map<CourseNo, CourseDesc>

type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

(* 
    1. Declare a function isValidCourseDesc: CourseDesc -> bool,
    where isValidCourseDesc desc is true if the ECTS part of desc is valid 
*)

let fp = ("Functional Programming", 5)
let sp = ("Strange Programming", 7)

let isValidCourseDesc (courseDesc: CourseDesc) = 
    let (_, ects) = courseDesc
    let valid = ects % 5 = 0
    valid

isValidCourseDesc fp
isValidCourseDesc sp

let validCourseBase: CourseBase = Map.ofList [02157, fp; 02158, ("An other course", 5)]
let invalidCourseBase: CourseBase = Map.ofList [02157, fp; 02158, ("An other course", 7)]

(*
    2. Declare a function isValidCourseBase: CourseBase -> bool,
    where isValidCourseBase cb is true if every course description occurring the course
    base cb is valid, that is, it satisfies the predicate isValidCourseDesc.
*)

let isValidCourseBase (courseBase: CourseBase) = 
    Map.forall(fun _ desc -> isValidCourseDesc desc) courseBase

isValidCourseBase validCourseBase
isValidCourseBase invalidCourseBase

(*
    3. Declare a function disjoint: Set<’a> -> Set<’a> -> bool, where disjoint s1 s2
    is true if the two sets s1 and s2 have no common element, that is, they are disjoint.
*)

let op: Optional = Set.ofList [02157; 02158]
let ma: Mandatory = Set.ofList [02141; 02131]

let disjoint s1 s2 = 
    let count = (Set.count s1) + (Set.count s2)
    let accCount = (Set.union s1 s2).Count
    count = accCount

disjoint op ma
disjoint op op
disjoint ma ma