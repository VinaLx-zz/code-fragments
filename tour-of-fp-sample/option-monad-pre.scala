case class Course()
case class Semester()
case class CourseInfo()
case class Student()
case class Grade()

object Simple {
  def getCourseById(id: Int): Option[Course] = Some(Course())
  def courseInfoOfSemester(
    info: CourseInfo, semester: Semester): Option[CourseInfo] =
      Some(CourseInfo())
  def getStudentById(studentId: Int): Option[Student] = Some(Student())
  def getGradeOfCourse(
    info: CourseInfo, student: Student): Option[Grade] = Some(Grade())

  def getGrade(
    courseId: Int, studentId: Int, semester: Semester): Option[Grade] = 
      for {
        course <- getCourseById(courseId)
        info <- courseInfoOfSemester(course, semester)
        student <- getStudentById(studentId)
        grade <- getGradeOfCourse(info, student)
      } yield grade
  
}
