package fcd

trait Syntax { self: Parsers & DerivedOps =>
  extension [R](p: Parser[R]) {
    def <<(in: Elem) = feed(p, in)
    def <<<(in: Seq[Elem]) = feedAll(p, in)
    def ~[U](q: Parser[U]) = seq(p, q)
    def <~[U](q: Parser[U]) = map(seq(p, q), _._1)
    def ~>[U](q: Parser[U]) = map(seq(p, q), _._2)
    def |[U >: R](q: Parser[U]) = alt(p, q)
    def &[U](q: Parser[U]) = and(p, q)
    def <&[U](q: Parser[U]) = map(and(p, q), _._1)
    def &>[U](q: Parser[U]) = map(and(p, q), _._2)

    // biased Alternative
    def <|[U >: R](q: Parser[U]) = biasedAlt(p, q)
    def |>[U >: R](q: Parser[U]) = biasedAlt(q, p)

    def ^^[U](f: R => U) = map(p, f)
    def ^^^[U](u: => U) = map(p, _ => u)
    def >>[U](f: R => Parser[U]) = flatMap(p, f)

    def ? = opt(p)
    def * = many(p)
    def + = some(p)
  }

  given liftToParser[R, U](using
      conv: R => U
  ): Conversion[Parser[R], Parser[U]] = map(_, conv)

  // tag nonterminals - this allows automatic insertion of nt-markers
  final case class NT[+R](parser: Parser[R])
  given [R]: Conversion[NT[R], Parser[R]] = _.parser

  import scala.language.implicitConversions
  implicit def toNT[R](parser: => Parser[R]): NT[R] = NT(nonterminal(parser))

  given tupleSeq3[T1, T2, T3, O]
      : Conversion[(T1, T2, T3) => O, (T1 ~ T2 ~ T3) => O] with {
    def apply(f: (T1, T2, T3) => O) = { case ((t1, t2), t3) => f(t1, t2, t3) }
  }

  given tupleSeq4[T1, T2, T3, T4, O]
      : Conversion[(T1, T2, T3, T4) => O, (T1 ~ T2 ~ T3 ~ T4) => O] with {
    def apply(f: (T1, T2, T3, T4) => O) = { case (((t1, t2), t3), t4) =>
      f(t1, t2, t3, t4)
    }
  }

  given tupleSeq5[T1, T2, T3, T4, T5, O]
      : Conversion[(T1, T2, T3, T4, T5) => O, (T1 ~ T2 ~ T3 ~ T4 ~ T5) => O]
  with {
    def apply(f: (T1, T2, T3, T4, T5) => O) = {
      case ((((t1, t2), t3), t4), t5) => f(t1, t2, t3, t4, t5)
    }
  }

  given tupleSeq6[T1, T2, T3, T4, T5, T6, O]: Conversion[
    (T1, T2, T3, T4, T5, T6) => O,
    (T1 ~ T2 ~ T3 ~ T4 ~ T5 ~ T6) => O
  ] with {
    def apply(f: (T1, T2, T3, T4, T5, T6) => O) = {
      case (((((t1, t2), t3), t4), t5), t6) => f(t1, t2, t3, t4, t5, t6)
    }
  }
}
