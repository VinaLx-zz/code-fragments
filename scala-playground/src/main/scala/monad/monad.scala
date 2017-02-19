package monad

trait Monad[F[_]] {
  def flatMap[A, B](ma: F[A])(f: A ⇒ F[B]): F[B]
  def unit[A](a: ⇒ A): F[A]

  def map[A, B](ma: F[A])(f: A ⇒ B): F[B] = {
    flatMap(ma)(a ⇒ unit(f(a)))
  }

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) ⇒ C) = {
    flatMap(ma)(a ⇒ map(mb)(b ⇒ f(a, b)))
  }

  def sequence[A](mas: List[F[A]]): F[List[A]] = {
    (mas :\ unit(Nil: List[A]))((ma, mla) ⇒ map2(ma, mla)(_ :: _))
  }
  def traverse[A, B](as: List[A])(f: A ⇒ F[B]): F[List[B]] = {
    (as :\ unit(Nil: List[B]))((a, mlb) ⇒ map2(f(a), mlb)(_ :: _))
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  def filterM[A](as: List[A])(f: A ⇒ F[Boolean]): F[List[A]] = as match {
    case Nil ⇒ unit(Nil)
    case head :: tail ⇒ flatMap(f(head)) { b ⇒
      if (b) map(filterM(tail)(f))(head :: _)
      else filterM(tail)(f)
    }
  }

  def compose[A, B, C](f: A ⇒ F[B])(g: B ⇒ F[C]): A ⇒ F[C] = {
    a ⇒ flatMap(f(a))(g)
  }

  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)(x ⇒ x)
  }
}