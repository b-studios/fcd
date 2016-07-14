package fcd

import language.higherKinds

trait Parsers {

  // the token type (`Elem`) and the type of the results are left abstract
  type Elem
  type Results[+R]

  // just an alias for tuple
  type ~[R, U] = (R, U)
  object ~ {
    def unapply[R, U](el: R ~ U): Option[(R, U)] = Some(el)
  }

  // The abstract interface of a parser
  type Parser[+R]

  def parse[R](p: Parser[R], in: Iterable[Elem]): Results[R]

  def fail: Parser[Nothing]
  def succeed[R](res: R): Parser[R]
  def acceptIf(cond: Elem => Boolean): Parser[Elem]

  def map[R, U](p: Parser[R], f: R => U): Parser[U]
  def flatMap[R, U](p: Parser[R], f: R => Parser[U]): Parser[U]
  def alt[R, U >: R](p: Parser[R], q: Parser[U]): Parser[U]
  def and[R, U](p: Parser[R], q: Parser[U]): Parser[(R, U)]
  def seq[R, U](p: Parser[R], q: Parser[U]): Parser[(R, U)]

  def feed[R](p: Parser[R], in: Elem): Parser[R]
  def feedAll[R](p: Parser[R], in: Iterable[Elem]): Parser[R] =
    in.foldLeft(p)(feed)

  // we also support negation in our implementation.
  def not[R](p: Parser[R]): Parser[Unit]

  def done[R](p: Parser[R]): Parser[R]
  def nonterminal[R](_p: => Parser[R]): Parser[R] = _p

  // For testing
  def isSuccess[R](p: Parser[R]): Boolean = !isFailure(p)
  def isFailure[R](p: Parser[R]): Boolean = !isSuccess(p)
  def accepts[R, ES <% Iterable[Elem]](p: Parser[R], s: ES): Boolean = isSuccess(feedAll(p, s))

  // As optimization
  def always: Parser[Unit]
}

trait RichParsers extends Parsers with Syntax with DerivedOps with CharSyntax

// A trait to bake parsers in a nested cake
trait ParserUsage {
  // Override _parsers in concrete tests suites with the
  // appropriate parser implementation.
  type Parsers
  def _parsers: Parsers
  lazy val parsers: Parsers = _parsers
}
