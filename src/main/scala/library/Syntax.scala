package fcd

import language.implicitConversions

trait Syntax { self: Parsers with DerivedOps =>

  implicit class ParserOps[R, P <% Parser[R]](p: P) {
    def <<(in: Elem): Parser[R] = self.feed(p, in)
    def <<<(in: Seq[Elem]): Parser[R] = self.feedAll(p, in)
    def parse(s: Seq[Elem]) = self.parse(p, s)

    def map[U](f: R => U): Parser[U] = self.map(p, f)
    def flatMap[U](f: R => Parser[U]): Parser[U] = self.flatMap(p, f)

    def ~[U](q: Parser[U]) = seq(p, q)
    def ~>[U](q: Parser[U]) = seq(p, q) map { case (a, b) => b }
    def <~[U](q: Parser[U]) = seq(p, q) map { case (a, b) => a }

    def |[U >: R](q: Parser[U]) = alt(p, q)

    def &[U](q: Parser[U]) = and(p, q)
    def <&[U](q: Parser[U]) = and(p, q) map { _._1 }
    def &>[U](q: Parser[U]) = and(p, q) map { _._2 }

    def ^^[U](f: R => U): Parser[U] = p map f
    def ^^^[U](u: => U): Parser[U] = p map { _ => u }

    def >>[U](f: R => Parser[U]): Parser[U] = p flatMap f

    def ? = opt(p)
    def * = many(p)
    def + = some(p)
  }

  implicit def liftToParsers[R, U](p: Parser[R])(implicit conv: R => U): Parser[U] =
    p map { conv }

  // tag nonterminals - this allows automatic insertion of nt-markers
  final case class NT[+R](parser: Parser[R])
  implicit def toParser[R](nt: NT[R]): Parser[R] = nt.parser
  implicit def toNT[R](parser: => Parser[R]): NT[R] = NT(nonterminal(parser))

  implicit def tupleSeq2[T1, T2, O](f: (T1, T2) => O): (T1 ~ T2) => O = {
    case t1 ~ t2 => f(t1, t2)
  }
  implicit def tupleSeq3[T1, T2, T3, O](f: (T1, T2, T3) => O): (T1 ~ T2 ~ T3) => O = {
    case t1 ~ t2 ~ t3 => f(t1, t2, t3)
  }
  implicit def tupleSeq4[T1, T2, T3, T4, O](f: (T1, T2, T3, T4) => O): (T1 ~ T2 ~ T3 ~ T4) => O = {
    case t1 ~ t2 ~ t3 ~ t4 => f(t1, t2, t3, t4)
  }
  implicit def tupleSeq5[T1, T2, T3, T4, T5, O](f: (T1, T2, T3, T4, T5) => O): (T1 ~ T2 ~ T3 ~ T4 ~ T5) => O = {
    case t1 ~ t2 ~ t3 ~ t4 ~ t5 => f(t1, t2, t3, t4, t5)
  }
  implicit def tupleSeq6[T1, T2, T3, T4, T5, T6, O](f: (T1, T2, T3, T4, T5, T6) => O): (T1 ~ T2 ~ T3 ~ T4 ~ T5 ~ T6) => O = {
    case t1 ~ t2 ~ t3 ~ t4 ~ t5 ~ t6 => f(t1, t2, t3, t4, t5, t6)
  }
}
