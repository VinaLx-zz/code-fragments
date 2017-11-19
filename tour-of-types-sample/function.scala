object Sorting {
  def Sort(a: Array[Int], compare: (Int, Int) => Boolean) = a sortWith compare

  def LessThan(i: Int, j: Int) = i < j
  def GreaterThan(i: Int, j: Int) = i > j

  def SortAscent(a: Array[Int]) = Sort(a, LessThan)
  def SortDescent(a: Array[Int]) = Sort(a, GreaterThan)

  def main(argv: Array[String]): Unit = {
    val arr = Array(2,1,3,5,4)
    println(SortAscent(arr) mkString " ")
    println(SortDescent(arr) mkString " ")
  }
}
