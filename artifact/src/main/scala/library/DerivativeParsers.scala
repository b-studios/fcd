package fcd

import scala.collection.mutable
import scala.collection.immutable.Set
import scala.util.DynamicVariable
import might.Attributed

trait DerivativeParsers extends Parsers { self: DerivedOps =>

  type Results[+R] = List[R]

  enum Parser[+R] extends Printable {
    case Fail() extends Parser[Nothing], NullaryPrintable("∅")
    case Always() extends Parser[Unit], NullaryPrintable("∞")
    case Succeed(res: R) extends Parser[R], NullaryPrintable("ε")
    case Done(p: Parser[R]) extends Parser[R], UnaryPrintable("done", p)
    case Accept(elem: Elem) extends Parser[Elem], NullaryPrintable("accept")
    case AcceptIf(p: Elem => Boolean)
        extends Parser[Elem],
        NullaryPrintable("acceptIf")
    case Not(p: Parser[R]) extends Parser[Unit], UnaryPrintable("not", p)
    case Alt(p: Parser[R], q: Parser[R])
        extends Parser[R],
        BinaryPrintable("|", p, q)
    case And[R, U](p: Parser[R], q: Parser[U])
        extends Parser[(R, U)],
        BinaryPrintable("&", p, q)
    case Seq[R, U](p: Parser[R], q: Parser[U])
        extends Parser[(R, U)],
        BinaryPrintable("~", p, q)
    case MapToUnit(p: Parser[Any])
        extends Parser[Unit],
        UnaryPrintable("mapToUnit", p)
    case FMapRes[R, U](p: Parser[R], f: R => Iterable[U])
        extends Parser[U],
        UnaryPrintable("fmap", p)
    case MapRes[R, U](p: Parser[R], f: R => U)
        extends Parser[U],
        UnaryPrintable("map", p)
    case FlatMap[R, U](p: Parser[R], f: R => Parser[U])
        extends Parser[U],
        UnaryPrintable("map", p)
    case NT[R](inner: Nonterminal[R], n: String = "nt")
        extends Parser[R],
        NullaryPrintable(n)

    def consume(x: Elem): Parser[R] = this match {
      case Fail() | Always()                              => this
      case Accept(elem) if x == elem                      => Succeed(x)
      case AcceptIf(f) if f(x)                            => Succeed(x)
      case Succeed(_) | Done(_) | Accept(_) | AcceptIf(_) => Fail()
      case Not(p)                                         => p.consume(x).not
      case Alt(p, q)     => p.consume(x).alt(q.consume(x))
      case And(p, q)     => p.consume(x).and(q.consume(x))
      case Seq(p, q)     => p.consume(x).seq(q).alt(p.done.seq(q.consume(x)))
      case MapToUnit(p)  => p.consume(x).mapToUnit
      case FMapRes(p, f) => p.consume(x).fmap(f)
      case MapRes(p, f)  => p.consume(x).map(f)
      case FlatMap(p, f) => {
        val next = p.consume(x).flatMap(f)
        p.results.foldLeft(next)((acc, y) => acc.alt(f(y).consume(x)))
      }
      case NT(inner, _) => inner.consume(x)
    }

    def accepts: Boolean = this match {
      case Fail() | Accept(_) | AcceptIf(_) => false
      case Always() | Succeed(_) | Done(_)  => true
      case Not(p)                           => !p.accepts
      case Alt(p, q)                        => p.accepts || q.accepts
      case And(p, q)                        => p.accepts && q.accepts
      case Seq(p, q)                        => p.accepts && q.accepts
      case MapToUnit(p)                     => p.accepts
      case FMapRes(p, _)                    => p.accepts
      case MapRes(p, _)                     => p.accepts
      case FlatMap(p, f)                    => p.results.exists(f(_).accepts)
      case NT(inner, _)                     => inner.accepts
    }

    def failed = this match {
      case Always() | Accept(_) | AcceptIf(_) | Done(_) | Succeed(_) => false
      case Fail()                                                    => true
      case Alt(p, q)     => p.failed && q.failed
      case And(p, q)     => p.failed || q.failed
      case Seq(p, q)     => p.failed // || q.failed
      case MapToUnit(p)  => p.failed
      case FMapRes(p, _) => p.failed
      case MapRes(p, _)  => p.failed
      case FlatMap(p, _) => p.failed
      case NT(inner, _)  => inner.failed
      case Not(_)        => false
    }

    def results: Iterable[R] = this match {
      case Fail() | Accept(_) | AcceptIf(_) => Set()
      case Always()                         => Set(())
      case Not(_) | MapToUnit(_) => if accepts then Set(()) else Set()
      case Succeed(res)          => Set(res)
      case Done(p)               => p.results
      case Alt(p, q)             => Set.from(p.results ++ q.results)
      case And(p, q)     => for x <- p.results; y <- q.results yield (x, y)
      case Seq(p, q)     => for x <- p.results; y <- q.results yield (x, y)
      case FMapRes(p, f) => Set.from(p.results.flatMap(f))
      case MapRes(p, f)  => Set.from(p.results.map(f))
      case FlatMap(p, f) => Set.from(p.results.flatMap(f(_).results))
      case NT(inner, _)  => inner.results
    }

    def done: Parser[R] = this match {
      case Fail() | Succeed(_) | Done(_) => this
      case Always()                      => Succeed(())
      case MapToUnit(p)                  => p.done.mapToUnit
      case FMapRes(p, f)                 => p.done.fmap(f)
      case MapRes(p, f)                  => p.done.map(f)
      case _ if accepts                  => Done(this)
      case _                             => Fail()
    }

    def not: Parser[Unit] = this match {
      case Always()      => Fail()
      case Fail()        => Always()
      case Not(p)        => p.mapToUnit
      case Alt(p, q)     => p.not.and(q.not).mapToUnit
      case And(p, q)     => p.not.alt(q.not)
      case MapToUnit(p)  => p.not
      case FMapRes(p, _) => p.not
      case MapRes(p, _)  => p.not
      case p             => Not(p)
    }

    def prefix: Parser[Unit] = this match {
      case p @ Fail()   => p
      case _ if accepts => Always()
      case _            => eat(consume(_).prefix)
    }

    def mapToUnit: Parser[Unit] = this match {
      case p @ (Fail() | Always() | Not(_) | MapToUnit(_)) => p
      case Succeed(_) | Done(_)                            => Succeed(())
      case FMapRes(p, _)                                   => p.mapToUnit
      case MapRes(p, _)                                    => p.mapToUnit
      case p                                               => MapToUnit(p)
    }

    def fmap[U](f: R => Iterable[U]): Parser[U] = this match {
      case p @ Fail()    => p
      case FMapRes(p, g) => p.fmap(x => g(x).flatMap(f))
      case MapRes(p, g)  => p.fmap(x => f(g(x)))
      case p             => FMapRes(p, f)
    }

    def map[U](f: R => U): Parser[U] = this match {
      case p @ Fail()    => p
      case FMapRes(p, g) => p.fmap(x => g(x).map(f))
      case MapRes(p, g)  => p.map(x => f(g(x)))
      case p             => MapRes(p, f)
    }

    def flatMap[U](f: R => Parser[U]): Parser[U] = this match {
      case p @ Fail()   => p
      case Succeed(res) => f(res)
      case Done(_)      => results.map(f).reduce(_ `alt` _)
      case FMapRes(p, g) =>
        p.flatMap(x => g(x).iterator.map(f).reduce(_ `alt` _))
      case MapRes(p, g) => p.flatMap(x => f(g(x)))
      case p            => FlatMap(p, f)
    }

    def alt[U >: R](q: Parser[U]): Parser[U] = (this, q) match {
      case (Fail(), y)       => y
      case (x, Fail())       => x
      case (x @ Always(), _) => x
      case (_, y @ Always()) => y
      case (x, y)            => Alt(x, y)
    }

    def and[U](q: Parser[U]): Parser[(R, U)] = (this, q) match {
      case (x @ Fail(), _) => x
      case (_, y @ Fail()) => y
      case (Always(), y)   => y.map(((), _))
      case (x, Always())   => x.map((_, ()))
      // canonicalization rule (2) from PLDI 2016
      case (FMapRes(x, f), y) =>
        x.and(y).fmap { case (r1, r2) => f(r1).map((_, r2)) }
      case (x, FMapRes(y, f)) =>
        x.and(y).fmap { case (r1, r2) => f(r2).map((r1, _)) }
      case (MapRes(x, f), y) =>
        x.and(y).map { case (r1, r2) => (f(r1), r2) }
      case (x, MapRes(y, f)) =>
        x.and(y).map { case (r1, r2) => (r1, f(r2)) }
      case (x, y) => And(x, y)
    }

    def seq[U](q: Parser[U]): Parser[(R, U)] = (this, q) match {
      case (x @ Fail(), _)  => x
      case (_, y @ Fail())  => y
      case (Succeed(r), y)  => y.map((r, _))
      case (x @ Done(_), y) => y.fmap(z => results.map((_, z)))
      // canonicalization rule (1) from PLDI 2016
      case (Seq(x, y), z) =>
        Seq(x, Seq(y, z)).map { case (r1, (r2, r3)) => ((r1, r2), r3) }
      // canonicalization rule (2) from PLDI 2016
      case (FMapRes(x, f), y) =>
        x.seq(y).fmap { case (r1, r2) => f(r1).map((_, r2)) }
      case (x, FMapRes(y, f)) =>
        x.seq(y).fmap { case (r1, r2) => f(r2).map((r1, _)) }
      case (MapRes(x, f), y) =>
        x.seq(y).map { case (r1, r2) => (f(r1), r2) }
      case (x, MapRes(y, f)) =>
        x.seq(y).map { case (r1, r2) => (r1, f(r2)) }
      case (x, y) => Seq(x, y)
    }
  }

  import Parser.*

  class Nonterminal[R](_p: => Parser[R], val name: String = "nt")
      extends Printable {
    lazy val p = _p

    def accepts = nullableFix.nullable.value
    def failed = emptyFix.empty.value
    def results: Set[R] = resultsFix.results.value

    // This separation into three fixed points is essential to
    // prevent excessive recomputation.

    private object emptyFix extends Attributed {
      object empty extends Attribute[Boolean](true, _ && _, follows)
      empty := p.failed
      protected def updateAttributes() = empty.update()
    }

    private object nullableFix extends Attributed {
      object nullable extends Attribute[Boolean](false, _ || _, implies)
      nullable := p.accepts
      protected def updateAttributes() = nullable.update()
    }

    private object resultsFix extends Attributed {
      object results
          extends Attribute[Set[R]](
            Set(),
            (nw, ol) => ol union nw,
            (nw, ol) => nw.subsetOf(ol)
          )

      results := Set.from(p.results)
      protected def updateAttributes() = results.update()
    }

    private val cache: mutable.HashMap[Elem, Parser[R]] = mutable.HashMap()
    // Wrapping in `nonterminal` is cecessary for left-recursive
    // grammars and for grammars like "DerivativeParsers / preprocessor"
    // that recursively derive. Optimizing the nonterminal node away causes
    // divergence on these grammars. Worse, in the latter case
    // forcing `next` will already cause divergence.
    def consume(x: Elem) = cache.getOrElseUpdate(
      x,
      if p.failed then Fail()
      else nonterminal(p.consume(x))
    )

    private val rec = DynamicVariable[Boolean](false)

    override def toString =
      if rec.value then s"nt(${System.identityHashCode(this)})"
      else rec.withValue(true) { s"nt($p)" }

    def printNode =
      if rec.value then ""
      else
        rec.withValue(true) {
          s"""  ${id} [shape=none, fillcolor="#dedede", style=filled, fontsize=8, fontname=mono, label=<$table>];
             |  ${id}:s -> ${p.id}
             |${p.printNode}""".stripMargin('|')
        }
  }

  // combinators without parser arguments
  val fail: Parser[Nothing] = Fail()
  val always: Parser[Unit] = Always()
  def succeed[R](res: R): Parser[R] = Succeed(res)
  def acceptIf(cond: Elem => Boolean): Parser[Elem] = AcceptIf(cond)

  // combinators with parser arguments
  def not[R](p: Parser[R]): Parser[Unit] = p.not
  def map[R, U](p: Parser[R], f: R => U) = p.map(f)
  def flatMap[R, U](p: Parser[R], f: R => Parser[U]) = p.flatMap(f)

  def alt[R, U >: R](p: Parser[R], q: Parser[U]) = q.alt(p)
  def seq[R, U](p: Parser[R], q: Parser[U]) = p.seq(q)
  def and[R, U](p: Parser[R], q: Parser[U]) = p.and(q)

  def feed[R](in: Elem, p: => Parser[R]) = p.consume(in)

  def results[R](p: Parser[R]) = List.from(p.results)

  def done[T](p: Parser[T]): Parser[T] = p.done

  override def nonterminal[R](_p: => Parser[R]) = NT(Nonterminal(_p))
  def nonterminal[R](name: String)(_p: => Parser[R]): Parser[R] =
    NT(Nonterminal(_p), name)

  def feed[R](p: Parser[R], in: Elem) = p.consume(in)
  def parse[R](p: Parser[R], in: Iterable[Elem]): Results[R] =
    List.from(feedAll(p, in).results)

  // for testing
  override def isSuccess[R](p: Parser[R]): Boolean = p.accepts
  override def accept(t: Elem): Parser[Elem] = Accept(t)

  // optimization: Once p accepts, p as a prefix will always accept.
  // often used to implement biased choice: (not(prefix(p)) &> q
  override def prefix: Parser[Any] => Parser[Unit] = _.prefix
}

object DerivativeParsers extends RichParsers with DerivativeParsers
