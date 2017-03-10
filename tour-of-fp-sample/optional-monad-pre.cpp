#include <experimental/optional>
#include "monad.h"

using std::experimental::optional;

namespace optional_monad {

template <typename A>
optional<A> unit(const A& a) {
    return optional<A>(a);
}
template <typename A, typename B>
optional<B> bind(const optional<A>& opt, function<optional<B>(A)> f) {
    if (opt) {
        return f(*opt);
    }
    return optional<B>{};
}

} // namespace optional_monad

class Course {};
class Semester {};
class CourseInfo {};
class Student {};
class Grade {};

optional<Course> GetCourseById(int course_id);
optional<CourseInfo> CourseInfoOfSemester(Course course, Semester semester);
optional<Student> GetStudentById(int student_id);
optional<Grade> GetGradeOfCourse(CourseInfo course, Student student);

optional<Grade> GetGrade(int course_id, int student_id, Semester semester) {
    auto course_opt = GetCourseById(course_id);
    if (course_opt) {
        auto info_opt = CourseInfoOfSemester(*course_opt, semester);
        if (info_opt) {
            auto student_opt = GetStudentById(student_id);
            if (student_opt) {
                return GetGradeOfCourse(*info_opt, *student_opt);
            }
        }
    }
    return {};
}

optional<Grade> GetGrade2(int course_id, int student_id, Semester semester) {
    using optional_monad::bind;
    return bind(GetCourseById(course_id), [&](Course course) {
        return bind(CourseInfoOfSemester(course, semester), [&](CourseInfo info) {
            return bind(GetStudentById(student_id), [&](Student s) {
                return GetGradeOfCourse(info, s);
            });
        });
    });
}
