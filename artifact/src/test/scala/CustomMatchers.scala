package fcd
package test

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.Tag

trait CustomMatchers[+P <: Parsers](val parsers: P) extends Matchers {
  self: AnyFunSpec =>

  import parsers.{Elem, Parser, accepts, isSuccess, parse}

  extension [T](p: => Parser[T]) {
    infix def shouldParse(s: Iterable[Elem], tags: Tag*) =
      it(s"""should parse "$s" """, tags*) {
        accepts(p, s) shouldBe true
      }
    infix def shouldNotParse(s: Iterable[Elem], tags: Tag*) =
      it(s"""should not parse "$s" """, tags*) {
        accepts(p, s) shouldBe false
      }
    // for unambiguous parses
    infix def shouldParseWith(s: Iterable[Elem], result: T) =
      it(s"""should parse "$s" with correct result""") {
        parse(p, s) shouldBe List(result)
      }
  }

  class SuccessMatcher[T] extends BeMatcher[Parser[T]] {
    def apply(left: Parser[T]) =
      MatchResult(
        isSuccess(left),
        left.toString + " was not successful",
        left.toString + " was successful"
      )
  }
}
