package fcd

import language.implicitConversions

trait CharSyntax { self: Parsers with DerivedOps with Syntax =>

  type Elem = Char

  implicit def charParser(c: Char): Parser[Char] = accept(c)

  def notChar(c: Char): Parser[Char] = acceptIf(_ != c)

  val char          = any
  val letter        = acceptIf(_.isLetter)
  val upper         = acceptIf(_.isUpper)
  val lower         = acceptIf(_.isLower)
  val whitespace    = acceptIf(_.isWhitespace)
  val digit         = acceptIf(_.isDigit)
  val letterOrDigit = acceptIf(_.isLetterOrDigit)
  val space         = acceptIf(_.isSpaceChar)
  val spaces        = many(space)
  val newline       = acceptIf(_ == '\n')

  def charRange(from: Char, to: Char) = acceptIf { c => c >= from && c <= to }

  val asciiLetter   = charRange('a', 'z') | charRange('A', 'Z')

  def string(s: String): Parser[String] = (acceptSeq(s) map (_.mkString))

  sealed trait Stringable[T] {
    def apply: T => String
  }
  object Stringable {
    implicit val char: Stringable[Char] = new Stringable[Char] {
      def apply = _.toString
    }
    implicit val charList: Stringable[List[Char]] = new Stringable[List[Char]] {
      def apply = _.mkString
    }
    implicit val string: Stringable[String] = new Stringable[String] {
      def apply = identity
    }
    implicit val stringList: Stringable[List[String]] = new Stringable[List[String]] {
      def apply = _.mkString
    }
    implicit def seq[T: Stringable, U: Stringable]: Stringable[T ~ U] = new Stringable[T ~ U] {
      def apply = { case l ~ r =>
        implicitly[Stringable[T]].apply(l) + implicitly[Stringable[U]].apply(r)
      }
    }
  }

  implicit def liftString(s: String): Parser[String] = string(s)

  implicit def charString(cs: List[Char]): String = cs.mkString

  implicit def stringParser[T: Stringable](p: Parser[T]): Parser[String] =
    p map { v => implicitly[Stringable[T]].apply(v) }

  def noneOf(s: String): Parser[Char] = acceptIf(t => !(s contains t))
}
