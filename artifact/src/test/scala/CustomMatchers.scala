package fcd
package test

import org.scalatest._
import org.scalatest.matchers._

trait CustomMatchers { self: FunSpec with Matchers =>

  // Due to initialization problems we have to use this pattern
  // of def and lazy val.
  //
  // Override _parsers in concrete tests suites with the
  // appropriate parser implementation.
  type Parsers = RichParsers
  def _parsers: RichParsers
  lazy val parsers = _parsers
  import parsers.{ Results, isSuccess, Parser, accepts, Elem }

  implicit class ParserTests[T, P <% Parser[T]](p: => P) {
    def shouldParse[ES <% Iterable[Elem]](s: ES, tags: Tag*) =
      it (s"""should parse "$s" """, tags:_*) {
        accepts(p, s) shouldBe true
      }
    def shouldNotParse[ES <% Iterable[Elem]](s: ES, tags: Tag*) =
      it (s"""should not parse "$s" """, tags:_*) {
        accepts(p, s) shouldBe false
      }
  }

  class SuccessMatcher extends BeMatcher[Parser[_]] {
    def apply(left: Parser[_]) =
      MatchResult(
        isSuccess(left),
        left.toString + " was not successful",
        left.toString + " was successful"
      )
  }
  lazy val successful = new SuccessMatcher
  lazy val failure = not (successful)
}
