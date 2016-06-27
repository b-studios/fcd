package fcd
package test

import org.scalatest._
import scala.language.higherKinds
import language.implicitConversions

class DerivativeParsersTests extends FunSpec with Matchers with CustomMatchers
    with BasicCombinatorTests
    with NegationTests
    with LeftrecTests
    with Section3 with Section4 with Section5 {

  def _parsers: DerivativeParsers.type = DerivativeParsers
  override lazy val parsers: DerivativeParsers.type = _parsers

  import parsers._

  // it is necessary to rename some combinators since names are already
  // bound by scala test.
  import parsers.{ fail => err, noneOf => nonOf, oneOf => one, not => neg }

  // This test illustrates how to write graph representations of the
  // parsers to a file. (To execute it replace `ignore` by `describe` and
  // run the tests.
  describe("printing graph representations of parsers") {
    lazy val num: Parser[Any] = many(digit)
    lazy val A: NT[Any] = B ~ '-' ~ num | num
    lazy val B: NT[Any] = succeed(()) ~ A

    A.printToFile("test.png")
  }

  describe("Examples in section 3") {
    import section_3_2._
    number shouldParse "42"
  }

  describe("Indentation with feed") {
    import section_3_4_improved._

    val xs = many(some('x') ~ '\n')
    indented(xs) shouldParse "  xxx\n  xxxx\n"
    indented(xs) shouldParse "      xxxxxxxxxx\n      xxxxxxxxxx\n"

    lazy val stmt: NT[Any] =
      ("while" ~ space ~ "(true):" ~ block
      | some('x') ~ '\n'
      )

    lazy val stmts = many(stmt)
    lazy val block: NT[Any] = '\n' ~ indented(stmts)
    stmt shouldParse "while (true):\n  xxxxx\n  xxxxx\n"
    stmt shouldParse "while (true):\n  while (true):\n    xxxxx\n  xxxx\n"
  }

  describe("Indentation with delegation") {
    import section_3_5_improved._

    val xs = many(some('x') ~ '\n')
    indented(xs) shouldParse "  xxx\n  xxxx\n"
    indented(xs) shouldParse "      xxxxxxxxxx\n      xxxxxxxxxx\n"

    lazy val stmt: NT[Any] =
      ("while" ~ space ~ "(true):" ~ block
      | some('x') ~ '\n'
      )

    lazy val stmts = many(stmt)
    lazy val block: NT[Any] = '\n' ~ indented(stmts)
    stmt shouldParse "while (true):\n  xxxxx\n  xxxxx\n"
    stmt shouldParse "while (true):\n  while (true):\n    xxxxx\n  xxxx\n"
  }

  describe("Simplified tables for paper") {
    import section_5_2.table

    lazy val xs = many(some('x') ~ '\n')

    table(xs) shouldParse """+---+
                            ^|xxx|
                            ^+---+
                            ^""".stripMargin('^')

    table(xs) shouldParse """+---+--------+------------+
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^+---+--------+------------+
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^+---+--------+------------+
                            ^""".stripMargin('^')
  }

  describe("Table parser with delegation") {

    type Layout = List[Int]

    // A parser computing the table layout
    lazy val head: Parser[Layout] = some('+'~> manyCount('-')) <~ '+' <~ '\n'


    def table[T](content: Parser[T]): Parser[List[List[T]]] = head >> { layout =>
      // After knowing the layout the row-separators are fixed
      val rowSeparator = layout.map { n => ("-" * n) + "+" }.foldLeft("+")(_+_) ~ '\n'
      val initCells    = layout.map { _ => content }

      // one line of a cell, given a fixed width.
      def cell: Int => Parser[T] => Parser[Parser[T]] = width => p =>
        (delegateN(width, p) <~ '|') ^^ { p => p << '\n' }

      // repeatAll is like repeat, but with a list of parsers as the state.
      val row = repeatAll[T] { ps =>
        '|' ~> distr(zipWith(layout map cell, ps)) <~ '\n'
      }

      some(row(initCells) <~ rowSeparator)
    }

    lazy val xs = many(some('x') ~ '\n')

    table(xs) shouldParse """+---+
                            ^|xxx|
                            ^+---+
                            ^""".stripMargin('^')

    table(xs) shouldParse """+---+--------+------------+
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^+---+--------+------------+
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                            ^+---+--------+------------+
                            ^""".stripMargin('^')

    table(xs) shouldNotParse """+---+--------+------------+
                               ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                               ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                               ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                               ^+---x--------+------------+
                               ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                               ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                               ^+---+--------+------------+
                               ^""".stripMargin('^')


    lazy val nestedTables: NT[Any] = table(xs | nestedTables)

    nestedTables shouldParse """+---+--------+------------+
                               ^|xxx|+-+----+|xxxxxxxxxxxx|
                               ^|xxx||x|xxxx||xxxxxxxxxxxx|
                               ^|xxx|+-+----+|xxxxxxxxxxxx|
                               ^+---+--------+------------+
                               ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                               ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                               ^+---+--------+------------+
                               ^""".stripMargin('^')

    nestedTables shouldNotParse """+---+--------+------------+
                                  ^|xxx|+-+----+|xxxxxxxxxxxx|
                                  ^|xxx||x|oxxx||xxxxxxxxxxxx|
                                  ^|xxx|+-+----+|xxxxxxxxxxxx|
                                  ^+---+--------+------------+
                                  ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                                  ^|xxx|xxxxxxxx|xxxxxxxxxxxx|
                                  ^+---+--------+------------+
                                  ^""".stripMargin('^')


    // helper that should be in the stdlib
    def zipWith[A,B](l1: List[A => B], l2: List[A]): List[B] =
      (l1 zip l2).map { case (f, x) => f(x) }
  }

  describe("flatMap uses fixed point computation") {
    lazy val fm: NT[Int] = succeed(1) | fm.flatMap { n => if (n < 5) succeed(n + 1) else err }

    fm.results.toSet shouldBe Set(1,2,3,4,5)
  }


  describe("Stream preprocessing") {
    lazy val ones: NT[Any] = succeed(()) | '1' ~ ones
    lazy val zeros: NT[Any] = succeed(()) | '0' ~ zeros

    lazy val oneszeros: Parser[Any] = '1' ~ '1' ~ '0' ~ '0'

    def bin(p: Parser[Any]): NT[Any] =
      done(p) | (('a' ~> bin(p << '1')) | ('b' ~> bin(p << '0')))

    ones shouldParse "1111"

    bin(ones).accepts
    bin(ones) shouldParse "aaaaa"
    bin(ones) shouldNotParse "aaaaab"
    bin(zeros) shouldParse "bbbbb"
    bin(zeros) shouldNotParse "bbbbba"
    bin(oneszeros) shouldParse "aabb"
    bin(oneszeros) shouldNotParse "aabbb"

    bin(ones) shouldNotParse ("b" * 50)
  }


  describe("Results of ambiguous parses") {
    lazy val A: NT[Any] = (A <~ '+') ~ A | digit

    def shouldParseWith(str: String)(expected: Set[Any]) {
      (A <<< str).results.toSet should be (expected)
    }

    shouldParseWith("3") { Set('3') }
    shouldParseWith("3+2") { Set(('3', '2')) }
    shouldParseWith("3+2+1") { Set(('3', ('2', '1')), (('3', '2'), '1')) }
  }


  // Usecase
  // -------
  // Standard example from data dependent parsing papers (like "One parser to rule them all",
  // "A new Method for dependent Parsing", ...)
  // (adopted form the latter)
  //
  // Could however also be implemented with monadic parsers, if we have the position in the
  // input stream. Benefit of our approach: Body parser never sees more than N characters.
  describe("IMAP") {

    val number = consumed(charRange('1', '9') ~ many(digit) | '0').map { _.mkString.toInt }

    val header: Parser[Int] =
      ('{' ~ space) ~> number <~ (space ~ '}')

    // repeated here for convenience (and specialized to this usecase)
    def feedNTimes[T](p: Parser[T])(n: Int): Parser[T] =
      if (n <= 0)
        done(p)
      else
        eat { c => feedNTimes(p << c)(n - 1) }

    def IMAP[T](body: Parser[T]): Parser[T] =
      header >> feedNTimes(body)

    IMAP(many('a')) shouldParse "{ 1 }a"
    IMAP(many('a')) shouldNotParse "{ 1 }"
    IMAP(many('a')) shouldNotParse "{ 1 }aa"
    IMAP(many('a')) shouldParse "{ 7 }aaaaaaa"
    IMAP(many('a')) shouldNotParse "{ 7 }aaaaaaaa"
    IMAP(many('a')) shouldNotParse "{ 7 }"
  }



  // Usecase. interleaving parsers
  def interleave[T, S](p: Parser[T], q: Parser[S]): Parser[(T, S)] =
      (done(p) & done(q)) | eat { c =>
        interleave(q, (p << c)) map { case (s, t) => (t, s) }
      }

  describe("interleaving two parsers") {
    val p = 'a' ~ 'a' ~ 'a'
    val q = 'b' ~ 'b' ~ 'b'

    interleave(p, q) shouldParse "ababab"
    interleave(p, q) shouldNotParse "abababab"
    interleave(p, q) shouldNotParse "abab"
    interleave(p, q) shouldNotParse "ab"
    interleave(p, q) shouldNotParse ""
  }

  // Usecase. Indentation that also skips empty lines
  def indent[T](p: Parser[T]): Parser[T] = {

    def readLine(p: Parser[T]): Parser[T] = done(p) | eat { c =>
      if (c == '\n') { indent(p << c) }
      else { readLine(p << c) }
    }

    done(p) |                                     // do not indent and p can accept
    (space ~ space) ~> readLine(p) |              // indent by 2 and read one line, then recurse
    (many(space) ~ newline) >> { _ => indent(p) } // skip lines with whitespace only, then recurse
  }


  describe("indenting parsers") {

    val xs = many(some('x') ~ '\n')

    indent(xs) shouldParse ""
    indent(xs) shouldParse "  xx\n"
    indent(xs) shouldParse "  xxxxx\n"
    indent(xs) shouldParse "  xxxxx\n  xxxxxxx\n"
    indent(xs) shouldParse """  xxxxx
                             |  xxxxxxx
                             |  xxxxxxxx
                             |  xxxxxxxxx
                             |  xxxxxxxxxx
                             |  xxxxxxxxxxx
                             |  xxxxxxxxxx
                             |  xxxxxxxxx
                             |  xxxxxxxxxx
                             |  xxxxxxxxxxx
                             |  xxxxxxxxxxxx
                             |  xxxxxxxxxxxxx
                             |  xxxxxxxxxxxxxx
                             |  xxxxxxxxxxxxxxx
                             |  xxxxxxxxxxxxxxxx
                             |  xxxxxxxxxxxxxxx
                             |  xxxxxxxxxxxxxx
                             |""".stripMargin('|')

    indent(indent(xs)) shouldParse "    xx\n"
    indent(indent(xs)) shouldParse "    xxxxx\n"
    indent(indent(xs)) shouldParse "    xxxxx\n    xxxxxxx\n"

    indent(indent(xs)) shouldParse "    xxxxx\n\n    xxxxxxx\n"
    indent(indent(xs)) shouldParse "    xxxxx\n \n    xxxxxxx\n"
    indent(indent(xs)) shouldParse "    xxxxx\n   \n\n   \n    xxxxxxx\n"
    indent(indent(xs)) shouldNotParse "   xxxxx\n   \n\n   \n    xxxxxxx\n"
    indent(indent(xs)) shouldNotParse "    xxxxx\n   \n\n   \n   xxxxxxx\n"
  }

  describe("Parens parser") {
    import section_5_2.parens
    parens shouldParse ""
    parens shouldParse "()"
    parens shouldParse "(())"
    parens shouldNotParse "(()"
  }

  describe("Retroactively, allow spaces in arbitrary positions") {
    import section_5_2.{ spaced, parens }
    val sp = spaced(parens)

    sp shouldParse "((()))"
    sp shouldParse "((( )))"
    sp shouldParse "( (( )))"
    sp shouldParse "( (( ))) "
    sp shouldParse "( (\n    (\n )) ) "
    sp shouldNotParse "( (    ( )) "
  }

  describe("Allowing parens in code blocks") {
    import section_5_2._

    as shouldParse "aaa\n"
    as shouldParse "\n"
    as shouldParse "aa\naa\n"

    both shouldParse "a\n"
    both shouldParse """aaa
                       |~~~
                       |()
                       |~~~
                       |aaaaa
                       |""".stripMargin('|')

    both shouldParse "a  \n\n~~~  \n()\n~~~\naaa\n"

    both shouldNotParse """aaa
                          |~~~
                          |(
                          |~~~
                          |aaaaa
                          |""".stripMargin('|')

    both shouldParse """aaa
                       |~~~
                       |((())
                       |~~~
                       |aaaaa
                       |
                       |~~~
                       |)
                       |~~~
                       |""".stripMargin('|')
  }



  describe("Unescape") {

    import section_5_2._

    unescape(many('\n')) shouldParse """\n\n\n"""
    unescape(many("\n" | "a")) shouldParse """\na\n\n"""
    unescape(many("\n" | "a")) shouldParse """\na\n\naaa"""
  }

  describe("Combined examples") {
    import section_5_2._
    combined shouldParse """aaa
                           ^""".stripMargin('^')

    combined shouldParse """+----+
                           ^|aaaa|
                           ^+----+
                           ^""".stripMargin('^')

    combined shouldParse """+----+
                           ^|aa  |
                           ^+----+
                           ^""".stripMargin('^')

    combined shouldParse """+----+
                           ^|aaaa|
                           ^|~~~ |
                           ^|(())|
                           ^|~~~ |
                           ^|aaaa|
                           ^+----+
                           ^""".stripMargin('^')

    combined shouldParse """+----+
                           ^|aa  |
                           ^|aaaa|
                           ^+----+
                           ^""".stripMargin('^')

    combined shouldParse """+----+
                           ^|aa  |
                           ^|~~~ |
                           ^|(())|
                           ^|~~~ |
                           ^|aaaa|
                           ^+----+
                           ^""".stripMargin('^')

  }

  describe("greedy repitition") {

    it ("should return only the result of the longest match") {
      greedySome(some('a')) parse "a"   shouldBe List(List(List('a')))
      greedySome(some('a')) parse "aaa" shouldBe List(List(List('a', 'a', 'a')))
    }

    it ("should also return longest match if other parser succeeded first") {
      lazy val p = some("ab") | some("a") | some("b")
      greedySome(p) parse "ab" shouldBe List(List(List("ab")))
      greedySome(p) parse "abab" shouldBe List(List(List("ab", "ab")))
      greedySome(p) parse "abbab" shouldBe List(List(List("ab"), List("b"), List("ab")))
      greedySome(p) parse "abbaab" shouldBe List(List(List("ab"), List("b"), List("a", "a"), List("b")))
      greedySome(p) parse "aaaab" shouldBe List(List(List("a", "a", "a", "a"), List("b")))

      lazy val q = "ab" | "a" | "b"
      greedySome(q) parse "ab" shouldBe List(List("ab"))
      greedySome(q) parse "abab" shouldBe List(List("ab", "ab"))
      greedySome(q) parse "abbab" shouldBe List(List("ab", "b", "ab"))
      greedySome(q) parse "abbaab" shouldBe List(List("ab", "b", "a", "ab"))
      greedySome(q) parse "aaaab" shouldBe List(List("a", "a", "a", "ab"))

    }

  }

}
