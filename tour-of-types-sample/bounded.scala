object Bounded {
  def Concat[A](as1: List[A], as2: List[A]): List[A] = {
    // ..
    as1 ++ as2
  }
  def Concat2[A, B <: A](as1: List[A], as2: List[B]): List[A] = {
    // ..
    as1 ++ as2
  }
  def Join[S <: String](ss: List[S]): String = {
    // ..
    ss mkString ""
  }
}
