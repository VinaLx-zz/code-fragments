package state

case class State[S, +A](run: S ⇒ (A, S)) {
  def apply(s: S) = run(s)
}

import monad._

object State {
  def monad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: ⇒ A): State[S, A] = State[S, A](s ⇒ (a, s))
    def flatMap[A, B](sa: State[S, A])(f: A ⇒ State[S, B]) = State[S, B] { s ⇒
      val (a, s1) = sa(s)
      f(a)(s1)
    }
  }
}