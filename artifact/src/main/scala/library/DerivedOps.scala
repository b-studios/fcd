package fcd

trait DerivedOps { self: Parsers with Syntax =>

  val any: Parser[Elem] = acceptIf(_ => true)

  def accept(t: Elem): Parser[Elem] = acceptIf(_ == t)

  def no(t: Elem): Parser[Elem] = acceptIf(_ != t)

  def acceptSeq[ES <% Iterable[Elem]](es: ES): Parser[List[Elem]] =
    es.foldRight[Parser[List[Elem]]](succeed(Nil)) { (x, pxs) =>
      accept(x) ~ pxs map mkList
    }

  def some[T](p: Parser[T]): Parser[List[T]] = {
    lazy val many_v: NT[List[T]] = alt(some_v, succeed(Nil))
    lazy val some_v: Parser[List[T]] = seq(p, many_v) map { case p ~ ps => p :: ps }
    some_v
  }
  def many[T](p: Parser[T]): Parser[List[T]] = {
    lazy val many_v: NT[List[T]] = alt(some_v, succeed(Nil))
    lazy val some_v: Parser[List[T]] = seq(p, many_v) map { case p ~ ps => p :: ps }
    many_v
  }

  // val always: Parser[Unit] = many(any) map { _ => () }

  // def always[T](t: T): Parser[T] =
  //   many(any) map { _ => t }

  def oneOf[ES <% Iterable[Elem]](s: ES): Parser[Elem] = acceptIf {
    t => s.exists(_ == t)
  }

  def noneOf[ES <% Iterable[Elem]](s: ES): Parser[Elem] = acceptIf {
    t => s.forall(_ != t)
  }

  def opt[T](p: Parser[T]): Parser[Option[T]] =
    alt(p map { r => Some(r) }, succeed(None))

  def manyN[T](n: Int, p: Parser[T]): Parser[List[T]] = {
    if (n == 0) succeed(Nil)
    else p ~ manyN(n - 1, p) map { case r ~ rs => r :: rs }
  }

  def manySep[T](p: Parser[T], sep: Parser[Any]): Parser[List[T]] = {
    alt(someSep(p, sep), succeed(Nil))
  }

  // same optimization as above for many and some
  def someSep[T](p: Parser[T], sep: Parser[Any]): Parser[List[T]] = {
    lazy val many_v: NT[List[T]] = alt(sep ~> some_v, succeed(Nil))
    lazy val some_v: Parser[List[T]] = seq(p, many_v) map { case p ~ ps => p :: ps }
    some_v
  }

  def manyCount(p: Parser[Any]): Parser[Int] =
    many(p) map { _.size }

  def someCount(p: Parser[Any]): Parser[Int] =
    some(p) map { _.size }

  // distributive law - chains a list of parsers
  // --> in Haskell one would use `traverse`
  def distr[T](ps: List[Parser[T]]): Parser[List[T]] =
    ps.foldRight(succeed[List[T]](Nil)) { (p, l) =>
      (p ~ l) map { case a ~ b => a :: b }
    }

  def join[T](p: Parser[Parser[T]]): Parser[T] = p flatMap done

  // A parser that captures the tokens consumed by `p`
  def consumed[T](p: Parser[T]): Parser[List[Elem]] =
    many(any) <& p

  def eat[R](f: Elem => Parser[R]): Parser[R] =
    any >> f

  def delegate[T](p: Parser[T]): Parser[Parser[T]] =
    succeed(p) | eat { c => delegate(p << c) }

  def delegateN[T](n: Int, p: Parser[T]): Parser[Parser[T]] =
    if (n <= 0)
      succeed(p)
    else
      eat { c => delegateN(n - 1, p << c) }

  // collects the results of parsers
  def collect[T](ps: List[Parser[T]]): Parser[List[T]] =
    ps.foldRight(succeed[List[T]](Nil)) { (p, l) =>
      done(p) >> { r => l.map(r :: _) }
    }

  def includes[T](p: Parser[T]): Parser[T] =
    many(any) ~> p <~ many(any)

  // A parser that repeatedly feeds input to the parser `p` in the context
  // described by the function `f`.
  def repeat[T](f: Parser[T] => Parser[Parser[T]]): Parser[T] => Parser[T] = p =>
    done(p) | f(p) >> repeat(f)

  // repeat is just an instance of repeatAll
  def repeatAll[T](f: List[Parser[T]] => Parser[List[Parser[T]]]): List[Parser[T]] => Parser[List[T]] = ps =>
    collect(ps) | f(ps) >> repeatAll(f)

  private def mkList[T] = (_: ~[T, List[T]]) match { case x ~ xs => x :: xs }

  val succeedForever: NT[Unit] =
    succeed(()) | (any ~> succeedForever)

  def rightDerivative[R](p: Parser[R], elem: Elem): Parser[R] =
    done(p << elem) | eat { c => rightDerivative(p << c, elem) }

  def rightDerivative[R](p: Parser[R], elems: Seq[Elem]): Parser[R] =
    done(p <<< elems) | eat { c => rightDerivative(p << c, elems) }


  // Greedy repitition
  private class Greedy[T](p: Parser[T]) {
      private def gs(curr: Parser[T]): Parser[List[T]] =
        // Either we are done and return a singleton list or we can process
        // another token.
        done(curr) ^^ { r => List(r) } | eat { el =>
          lazy val next = curr << el
          ( gs(next)
            // The remainder may only be processed with greedyMany if no
            // prefix may be recognized by `next`.
          | done(curr) ~ (not(next ~ always(())) &> (many << el)) ^^ {
              case fst ~ rst => fst :: rst
            }
          )
        }

      lazy val some: NT[List[T]] = gs(p)
      lazy val many: NT[List[T]] = some | succeed(Nil)
    }

    def greedySome[T](p: Parser[T]): Parser[List[T]] = new Greedy(p).some
    def greedyMany[T](p: Parser[T]): Parser[List[T]] = new Greedy(p).many

}
