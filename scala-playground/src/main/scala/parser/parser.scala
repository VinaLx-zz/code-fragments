package parser

case class Parser[+A] {
  import Parser._

  def flatMap(f: A ⇒ Parser[B]): Parser[B]
  def map(f: A ⇒ B): Parser[B] = {
    flatMap(a ⇒ unit(f(a)))
  }
}

import monad._

object Parser {
  def unit(a: ⇒ A): Parser[A]
  def monad: Monad[Parser] = new Monad {
    def unit[A](a: ⇒ A): Parser[A] = Parser.unit(a)
    def flatMap[A, B](pa: Parser[A])(f: A ⇒ Parser[B]) = pa flatMap f
  }
}