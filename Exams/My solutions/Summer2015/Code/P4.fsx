type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS
type CourseBase = Map<CourseNo, CourseDesc>

let cd = ("FP", 5)
let cd2 = ("FP", 6)

let isValidCourseDesc cd =
    let (title, ects) = cd
    ects > 0 && ects % 5 = 0

isValidCourseDesc cd
isValidCourseDesc cd2


let isValidCourseBase cb =
    Map.forall (fun _ cd -> isValidCourseDesc cd) cb
