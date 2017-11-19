object GenericSorting {
  def Sort[T](a: Array[T], compare: (T, T) => Boolean) = {
    // ..
    a sortWith compare
  }
  def IntAscent(i: Int, j: Int) = i > j
  def StringAscentByLength(i: String, j: String) = i.length > j.length

  def main(args: Array[String]): Unit = {
    val a1 = Array(2, 3, 1, 4, 5)
    val a2 = Array("hello", "generic", "sort")

    // 5 4 3 2 1
    println(Sort(a1, IntAscent) mkString " ")
    // generic hello sort
    println(Sort(a2, StringAscentByLength) mkString " ")
  }
}
