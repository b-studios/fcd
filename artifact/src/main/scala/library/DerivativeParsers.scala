package fcd

import scala.collection.mutable
import scala.util.DynamicVariable
import might.Attributed

trait DerivativeParsers extends Parsers { self: DerivedOps =>

  type Results[+R] = List[R]

  trait Parser[+R] extends Printable { p =>

    def results: Results[R]
    def consume: Elem => Parser[R]

    def accepts: Boolean
    def failed: Boolean

    def alt[U >: R](q: Parser[U]): Parser[U] = q alt2 p
    def alt2[U >: R](q: Parser[U]): Parser[U] = new Alt(q, p)
    def and[U](q: Parser[U]): Parser[(R, U)] = q and2 p
    def and2[U](q: Parser[U]): Parser[(U, R)] = new And(q, p)
    def seq[U](q: Parser[U]): Parser[R ~ U] = q seq2 p
    def seq2[U](q: Parser[U]): Parser[U ~ R] = new Seq(q, p)
    def flatMap[U](f: R => Parser[U]): Parser[U] = new FlatMap(p, f)
    def done: Parser[R] = if (accepts) Succeed(p.results) else fail

    def not: Parser[Unit] = new Not(p)

    // the map family
    def mapResults[U](f: (=> Results[R]) => Results[U]): Parser[U] = new MapResults(p, f)
    def map[U](f: R => U): Parser[U] = p mapResults { ress => ress map f }
    def withResults[U](res: List[U]): Parser[U] = mapResults(_ => res)

    // for optimization of biased choice
    def prefix: Parser[Unit] = {
       if (accepts) {
        always
      } else {
        eat { el => (p consume el).prefix }
      }
    }
  }

  object Fail extends NullaryPrintable("∅") with Parser[Nothing] {
    override def results = List.empty
    override def failed  = true
    override def accepts = false
    override def consume: Elem => this.type = in => this

    override def alt[U >: Nothing](q: Parser[U]): q.type = q
    override def alt2[U >: Nothing](q: Parser[U]): q.type = q
    override def seq[U](q: Parser[U]): this.type = this
    override def seq2[U](q: Parser[U]): this.type = this
    override def and[U](q: Parser[U]): this.type = this
    override def and2[U](q: Parser[U]): this.type = this
    override def map[U](f: Nothing => U): this.type = this
    override def flatMap[U](g: Nothing => Parser[U]): this.type = this
    override def mapResults[U](f: (=> Results[Nothing]) => Results[U]): this.type = this
    override def done = this

    override def not: Parser[Unit] = Always
    override def prefix = this
    override def toString: String = "∅"
  }

  object Always extends NullaryPrintable("∞") with Parser[Unit] {
    override def results = List(())
    override def failed  = false
    override def accepts = true
    override def consume = in => Always
    override def not: Parser[Unit] = fail
    override def and[U](q: Parser[U]): Parser[(Unit, U)] = q map { r => ((), r) }
    override def and2[U](q: Parser[U]): Parser[(U, Unit)] = q map { r => (r, ()) }

    // this is a valid optimization, however it almost never occurs.
    override def alt[U >: Unit](q: Parser[U]) = this
    override def alt2[U >: Unit](q: Parser[U]) = this
    override def toString = "always"
  }

  case class Succeed[R](ress: Results[R]) extends NullaryPrintable("ε") with Parser[R] { p =>
    override def results = ress
    override def failed  = false
    override def accepts = true
    override def consume = (in: Elem) => fail
    override def toString = s"ε($ress)"
    override def done: Parser[R] = this
    override def mapResults[T](f: (=> Results[R]) => Results[T]): Parser[T] = Succeed(f(ress))
    override def seq[U](q: Parser[U]): Parser[R ~ U] = q mapResults { ress2 =>
      for (r <- ress; r2 <- ress2) yield (r, r2)
    }
    override def seq2[U](q: Parser[U]): Parser[U ~ R] = q mapResults { ress2 =>
      for (r <- ress; r2 <- ress2) yield (r2, r)
    }
    override def flatMap[U](f: R => Parser[U]): Parser[U] = ress.map(f).reduce(_ alt _)
  }

  case class Accept(elem: Elem) extends Parser[Elem] {
    def results = List.empty
    def failed  = false
    def accepts = false
    def consume = (in: Elem) =>
      if (in == elem) {
        succeed(in)
      } else {
        fail
      }

    lazy val name = "'" + escape(elem) + "'"
    def printNode = s"""$id [label="$name", shape=circle]"""
    private def escape(c: Elem): String = c.toString.replace("\\", "\\\\").replace("\"", "\\\"")
  }

  class AcceptIf(f: Elem => Boolean) extends NullaryPrintable("acceptIf") with Parser[Elem] {
    def results = List.empty
    def failed  = false
    def accepts = false
    def consume = (in: Elem) =>
      if (f(in)) {
        succeed(in)
      } else {
        fail
      }
  }

  class Not[R](val p: Parser[R]) extends UnaryPrintable("not", p) with Parser[Unit] {
    def results = (if (p.results.isEmpty) List(()) else List.empty)
    def failed  = false // we never know, this is a conservative approx.
    def accepts = !p.accepts
    def consume: Elem => Parser[Unit] = in => (p consume in).not
    override def not = p withResults List(())
    override def toString = s"not($p)"
  }

  class Alt[R, U >: R](val p: Parser[R], val q: Parser[U]) extends BinaryPrintable("|", p, q) with Parser[U] {
    def results = (p.results ++ q.results).distinct
    def failed  = p.failed && q.failed
    def accepts = p.accepts || q.accepts
    def consume = (in: Elem) => (p consume in) alt (q consume in)
    override def toString = s"($p | $q)"
  }

  class Seq[R, U](val p: Parser[R], val q: Parser[U]) extends BinaryPrintable("~", p, q) with Parser[R ~ U] {

    def results = (for { r <- p.results; u <- q.results } yield (new ~(r, u))).distinct
    // q.failed forces q, which might not terminate for grammars with
    // infinite many nonterminals, like:
    //   def foo(p) = 'a' ~ foo(p << 'a')
    // so we approximate similar to flatmap.
    def failed  = p.failed // || q.failed
    def accepts = p.accepts && q.accepts
    def consume = (in: Elem) => ((p consume in) seq q) alt (p.done seq (q consume in))
    override def toString = s"($p ~ $q)"

    // canonicalization rule (1) from PLDI 2016
    override def seq[T](r: Parser[T]): Parser[(R ~ U) ~ T] =
      (p seq (q seq r)) map {
        case (rr ~ (ru ~ rt)) => ((rr, ru), rt)
    }
  }

  class Done[R](val p: Parser[R]) extends UnaryPrintable(s"done", p) with Parser[R] {
    def results = p.results
    def failed  = p.failed
    def accepts = p.accepts
    def consume = (el: Elem) => fail
    override def done = this
    override def toString = s"done($p)"
  }

  class MapResults[R, U](val p: Parser[R], f: (=> Results[R]) => Results[U]) extends UnaryPrintable(s"mapResults", p) with Parser[U] {
    def results = f(p.results).distinct
    def failed  = p.failed
    def accepts = p.accepts
    def consume = (el: Elem) => (p consume el) mapResults f
    override def mapResults[T](g: (=> Results[U]) => Results[T]): Parser[T] = p mapResults { res => g(f(res)) }
    override def map[T](g: U => T): Parser[T] = p mapResults { res => f(res) map g }
    override def done = p.done mapResults f
    override def not = p.not
    override def toString = s"map($p)"

    // canonicalization rule (2) from PLDI 2016
    // allows for instance rewriting (always.map(f) & p) -> p.map(...f...)
    override def seq[S](q: Parser[S]): Parser[U ~ S] =
      (p seq q).mapResults(rss => rss.unzip match {
        case (us, ss) => f(us) zip ss
      })
    override def seq2[S](q: Parser[S]): Parser[S ~ U] =
      (p seq2 q).mapResults(rss => rss.unzip match {
        case (ss, us) => ss zip f(us)
      })
    override def and[S](q: Parser[S]): Parser[(U, S)] =
      (p and q).mapResults(rss => rss.unzip match {
        case (us, ss) => f(us) zip ss
      })
    override def and2[S](q: Parser[S]): Parser[(S, U)] =
      (p and2 q).mapResults(rss => rss.unzip match {
        case (ss, us) => ss zip f(us)
      })
  }

  class And[R, U](val p: Parser[R], val q: Parser[U]) extends BinaryPrintable("&", p, q) with Parser[(R, U)] {
    def results = (for { r <- p.results; u <- q.results } yield ((r, u))).distinct
    def failed  = p.failed || q.failed
    def accepts = p.accepts && q.accepts
    def consume = (in: Elem) => (p consume in) and (q consume in)
    override def toString = s"($p & $q)"
  }

  class FlatMap[R, U](val p: Parser[R], f: R => Parser[U]) extends UnaryPrintable("flatMap", p) with Parser[U] {
    def results = ((p.results map f) flatMap (_.results)).distinct //res().distinct
    def accepts = !results.isEmpty
    def failed  = p.failed // that's the best we know

    def consume: Elem => Parser[U] = in => {
      val next = (p consume in) flatMap f
      val qss = (p.results map f) map (_ consume in)
      qss.foldLeft(next)(_ alt _)
    }

    // distribute not over flatMap
    // override def not = p.not alt (p flatMap (r => f(r).not))
    override def toString = "flatMap"
  }

  class Nonterminal[+R](_p: => Parser[R]) extends Parser[R] {
    lazy val p = _p

    def accepts: Boolean    = nullable.value
    def results: Results[R] = resultsAttr.value
    def failed: Boolean     = empty.value

    protected[this] object fix1 extends Attributed {
      override protected[this] def updateAttributes() {
        empty.update()
        nullable.update()
      }
    }

    protected[this] object fix2 extends Attributed {

      object results extends Attribute[List[R]](
        List.empty,
        (nw, ol) => (nw ++ ol).distinct,
        (nw, ol) => nw.toSet.subsetOf(ol.toSet))


      override protected[this] def updateAttributes() {
        results.update()
      }
    }

    val resultsAttr = fix2.results
    val empty = fix1.empty
    val nullable = fix1.nullable

    resultsAttr := p.results
    empty       := p.failed
    nullable    := p.accepts


    // The second try allows to recognize
    // b, ab,aab, ... with
    // lazy val A: NT[_] = "aaaab" | A << 'a'
    sealed trait DerivationResult[+R]
    case class Stable[+R](result: Parser[R]) extends DerivationResult[R]
    case object PreliminaryFail extends DerivationResult[Nothing]

    private[this] val cache: mutable.ListMap[Elem, DerivationResult[R]] = mutable.ListMap.empty
    protected[this] def compute(el: Elem): Parser[R] = {
      lazy val next: Parser[R] = p consume el
      lazy val nt: Parser[R] = {
        nonterminal(next)
      }
      // this forces p, which might lead to a diverging derivation
      // so we first need to memoize preliminary fail.
      if (p.failed)
        fail
      else
        nt
    }
    override def consume: Elem => Parser[R] = el =>
      cache.getOrElseUpdate(el, {
        cache(el) = PreliminaryFail;
        Stable(compute(el))
      }) match {
        case PreliminaryFail  =>
          cache(el) = Stable(fail)
          val res = compute(el)
          cache(el) = Stable(res)
          res
        case Stable(p) => p
      }

    def named(str: => String): this.type = {
      name = str
      this
    }
    var name = "nt"
    private val rec = new DynamicVariable[Boolean](false)
    override def toString =
      if (rec.value)
        s"nt(${System.identityHashCode(this)})"
      else
        rec.withValue(true) { s"nt($p)" }

    def printNode =
      if (rec.value)
        ""
      else
        rec.withValue(true) {
          s"""  ${id} [shape=none, fillcolor="#dedede", style=filled, fontsize=8, fontname=mono, label=<$table>];
             |  ${id}:s -> ${p.id}
             |${p.printNode}""".stripMargin('|')
        }
  }

  // combinators without parser arguments
  val fail: Parser[Nothing] = Fail
  val always: Parser[Unit] = Always
  def succeed[R](res: R): Parser[R] = Succeed(List(res))
  def acceptIf(cond: Elem => Boolean): Parser[Elem] = new AcceptIf(cond)

  // combinators with parser arguments
  def not[R](p: Parser[R]): Parser[Unit] = p.not
  def map[R, U](p: Parser[R], f: R => U) = p map f
  def flatMap[R, U](p: Parser[R], f: R => Parser[U]) = p flatMap f

  def alt[R, U >: R](p: Parser[R], q: Parser[U]) = p alt q
  def seq[R, U](p: Parser[R], q: Parser[U]) = p seq q
  def and[R, U](p: Parser[R], q: Parser[U]): Parser[(R, U)] = p and q

  def feed[R](in: Elem, p: => Parser[R]) = p consume in

  def results[R](p: Parser[R]) = p.results

  def done[T](p: Parser[T]): Parser[T] = p.done

  override def nonterminal[R](_p: => Parser[R]): Nonterminal[R] = new Nonterminal(_p)
  def nonterminal[R](name: String)(_p: => Parser[R]): Nonterminal[R] = new Nonterminal(_p).named(name)

  def feed[R](p: Parser[R], in: Elem) = p.consume(in)
  def parse[R](p: Parser[R], in: Iterable[Elem]): Results[R] = feedAll(p, in).results

  // for testing
  override def isSuccess[R](p: Parser[R]): Boolean = p.accepts
  override def accept(t: Elem): Parser[Elem] = Accept(t)

  // optimization: Once p accepts, p as a prefix will always accept.
  // often used to implement biased choice: (not(prefix(p)) &> q
  override def prefix: Parser[Any] => Parser[Unit] = p => p.prefix
}

object DerivativeParsers extends RichParsers with DerivativeParsers {
  override type Elem = Char
}
