// Problem 4 from the exam summer 2015

type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS

type CourseBase = Map<CourseNo, CourseDesc>

type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

type BasicNaturalScience = CourseGroup
type TechnologicalCore = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective = CourseNo -> bool
type FlagModel = BasicNaturalScience * TechnologicalCore * ProjectProfessionalSkill * Elective
type CoursePlan = Set<CourseNo>

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

(* 
    4. Declare a function sumECTS: Set<CourseNo> -> CourseBase -> int,
    where sumECTS cs cb is the sum of all ECTS points of the courses with numbers in cs,
    where the ECTS points are extracted from course descriptions in the course base cb.
*)

let courseNos = Set.ofList [02157; 02131]
let courseNos1 = Set.ofList [02157; 02158]
let courseBase: CourseBase = Map.ofList [02157, ("Functional Programming", 5); 02131, ("Embedded Systems", 10); 02141, ("Computer Science Modelling", 10); 02158, ("Parallel programming", 5)]

let sumECTS (cs: Set<CourseNo>, cb: CourseBase) = 
    Map.fold (fun acc no desc -> 
                let (_, ects) = desc
                if cs.Contains no then ects + acc else acc
                ) 0 cb

sumECTS (courseNos, courseBase)
sumECTS (courseNos1, courseBase)

(*
    5. A course group (man, opt) for a bachelor programme is valid for a given course base cb
    if:
    • man and opt are disjoint,
    • the sum of all mandatory ECTS points (i.e. the ECTS sum for all courses in man)
    is less than or equal to 45,

    manTotalrule:
    • the set of optional courses opt is empty when the mandatory ECTS points add up
    to 45, and
    • the total number of ECTS points of mandatory and optional courses should be at
    least 45.

    Declare a function isValidCourseGroup: CourseGroup -> CourseBase -> bool that
    can check whether a course group is valid for a given course base.
*)

let isValidCourseGroup (cg: CourseGroup, cb: CourseBase) =
    let (man, opt) = cg
    let res = disjoint man opt
    let optSum = sumECTS (opt, cb)
    let manSum = sumECTS (man, cb)
    let mandatoryECTSpoints = manSum <= 45 
    let totalSum = manSum + optSum
    let manTotalrule = opt.IsEmpty && manSum >= 45 && totalSum >= 45
    
    res && mandatoryECTSpoints && manTotalrule

(*
    6. Declare a function isValid: FlagModel -> CourseBase -> bool that can test whether
    a flag model is valid for a given course base.

    A course plan cs satisfies a (valid) flag model of a bachelor programme (for a given course
    base), if the number of ECTS points earned from the courses in cs is 180, subject to the
    requirement that 45 points are earned in each course group of the flag model, including
    the elective courses.
*)

// let isValid fm cb = 
