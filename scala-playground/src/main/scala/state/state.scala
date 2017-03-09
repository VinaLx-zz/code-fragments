package state

case class State[S, +A](run: S ⇒ (A, S)) {
  def apply(s: S) = run(s)

  def flatMap[B](f: A ⇒ State[S, B]) = State[S, B] { s ⇒
    val (a, s1) = run(s)
    f(a)(s1)
  }
  def map[B](f: A ⇒ B) = State[S, B] { s ⇒
    val (a, s1) = run(s)
    (f(a), s1)
  }
  def map2[B, C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] = {
    flatMap(a ⇒ sb.map(b ⇒ f(a, b)))
  }

  @annotation.tailrec
  final def go(s: S)(p: (A, S) ⇒ Boolean): (A, S) = run(s) match {
    case result if p.tupled(result) ⇒ result
    case (_, next) ⇒ go(next)(p)
  }
}

import monad._

object State {
  def monad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: ⇒ A): State[S, A] = State[S, A](s ⇒ (a, s))
    def flatMap[A, B](sa: State[S, A])(f: A ⇒ State[S, B]) = sa flatMap f
  }

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    monad[S].sequence(l)
  }

  def get[S] = State[S, S](s ⇒ (s, s))
  def set[S](s: ⇒ S) = State[S, Unit](_ ⇒ ((), s))
}