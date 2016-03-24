package fcd
package test

import org.scalatest._
import scala.language.higherKinds
import language.implicitConversions

class DerivativeParsersTests extends FunSpec with Matchers with CustomMatchers
    with BasicCombinatorTests
    with NegationTests
    with LeftrecTests {

  def _parsers: DerivativeParsers.type = DerivativeParsers
  override lazy val parsers: DerivativeParsers.type = _parsers

  import parsers._

  // it is necessary to rename some combinators since names are already
  // bound by scala test.
  import parsers.{ fail => err, noneOf => nonOf }

  lazy val space = some(" ")
  lazy val spaces = many(" ")
  lazy val name: Parser[String] = some(asciiLetter)
  def strip[T](p: Parser[T]): Parser[T] = spaces ~> (p <~ spaces)

  ignore("printing graph representations of parsers") {
    lazy val num: Parser[Any] = many(digit)
    lazy val A: NT[Any] = B ~ '-' ~ num | num
    lazy val B: NT[Any] = succeed(()) ~ A

    A.printToFile("test.png")
  }

  describe("Examples in section 2") {
    val digit: Parser[Int] = acceptIf(_.isDigit) ^^ { s => Integer.valueOf(s.toString) }

    lazy val number: NT[Int] =
      ( number ~ digit ^^ { case (n, d) => (n * 10) + d }
      | digit
      )

    number shouldParse "42"
  }

  describe("Indentation with feed") {

    def indentBy[T](n: Int): Parser[T] => Parser[T] = p =>
      done(p) | manyN(n, space) ~> readLine(n)(p)

    def readLine[T](n: Int)(p: Parser[T]): Parser[T] =
      ( nonOf("\n")  >> { c => readLine(n)(p << c) }
      | accept('\n') >> { c => indentBy(n)(p << c) }
      )

    def indented[T](p: Parser[T]): Parser[T] = consumed(some(space)) >> { case s =>
      // this simulates lookahead for greedy matching
      nonOf(" ") >> { c => indentBy(s.size)(p) <<< s << c }
    }

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
    lazy val line = (many(nonOf("\n")) <~ newline)
    def indentBy[T](n: Int): Parser[T] => Parser[T] = p =>
      done(p) | manyN(n, space) ~> (line &> delegate(p)) >> indentBy(n)

    def indented[T](p: Parser[T]): Parser[T] = consumed(some(space)) >> { case s =>
      // this simulates lookahead for greedy matching
      nonOf(" ") >> { c => indentBy(s.size)(p) <<< s << c }
    }

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

    type Layout = List[Int]

    def table[T](cell: Parser[T]): Parser[List[List[T]]] =
      (head <~ '\n') >> { layout => body(layout, cell) }

    // a parser computing the table layout
    def head: Parser[Layout] = some('+'~> manyCount('-')) <~ '+'

    def body[T](layout: Layout, cell: Parser[T]): Parser[List[List[T]]] =
      many(rowLine(layout, layout.map(n => cell)) <~ rowSeparator(layout))

    // given a layout, creates a parser for row separators
    def rowSeparator(layout: Layout): Parser[Any] =
      layout.map { n => ("-" * n) + "+" }.foldLeft("+")(_+_) ~ '\n'

    // either read another rowLine or quit cell parsers and collect results
    def rowLine[T](layout: Layout, cells: List[Parser[T]]): Parser[List[T]] =
      ( ('|' ~> distr(delegateCells(layout, cells)) <~ '\n') >> { cs => rowLine(layout, cs) }
      | collect(cells)
      )

    // first feed n tokens to every cell parser, then feed newline and read a pipe
    def delegateCells[T](layout: Layout, cells: List[Parser[T]]): List[Parser[Parser[T]]] =
      layout.zip(cells).map {
        case (n, p) => delegateN(n, p).map(_ << '\n') <~ '|'
      }


    // The only parser that makes use of `<<` is delegate Cells, all others just use standard combinators
    // 1. feed n tokens to p
    // 2. feed newline and read bar


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



  // Usecase 1.
  // ----------
  // interleaving parsers
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

  // Usecase 2.
  // ----------
  // modular indentation
  // this can be cached the same way as `many` and `some`

  // Character based dispatch
  def dispatch[T](cs: (Elem, Elem => Parser[T])*)(default: Elem => Parser[T]): Parser[T] =
    eat { el =>
      cs find { case (c, f) => c == el } match {
        case Some((_, f)) => f(el)
        case None => default(el)
      }
    }

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


  // the above indentation does not allow ignoring indentation in braces / multiline strings
  describe("lexical aware indentation") {
    def indent[T](lexeme: Parser[String])(p: Parser[T]): Parser[T] = {

      def readLine(p: Parser[T]): Parser[T] =
        done(p) | lexeme >> { lex =>
          if (lex == "\n") { indent(lexeme)(p <<< lex) }
          else { readLine(p <<< lex) }
        }

      done(p) | (space ~ space) ~> readLine(p)
    }

    val name: Parser[String] = some(letter)
    val str: Parser[String] = consumed('"' ~ many(nonOf("\"")) ~ '"')

    // lexemes are not terminated by space/newline/eos, so for xxx flatmap above will be called
    // three times (x, xx, xxx). In an extension with lookahead, this could be fixed.
    val lexeme: Parser[String] = name | " " | "\n" | str

    val xs = many(some("x" | str) ~ '\n')

    lazy val ind = indent(lexeme)(xs)

    ind shouldParse "    xx\n"
    ind shouldParse "    xxxxx\n"
    ind shouldParse "    xxxxx\n    xxxxxxx\n"

    (ind <<< "    xxxxx\n    xxxxxxx\n").results.size shouldBe 1

    ind shouldParse "    xxx\"\"xx\n    xxxxxxx\n"
    ind shouldParse "    xxx\"   \n    \n\n   \"xx\n    xxxxxxx\n"
    ind shouldNotParse "    xxx\"   \n    \n\nxx\n    xxxxxxx\n"
    ind shouldNotParse "    xxxxx\n xxxx\n"
  }


  lazy val parens: NT[Any] = '(' ~ parens ~ ')' | succeed(())


  describe("Parens parser") {
    parens shouldParse ""
    parens shouldParse "()"
    parens shouldParse "(())"
    parens shouldNotParse "(()"
  }

  def spaced[T]: Parser[T] => Parser[T] = p =>
    done(p) | eat {
      case ' ' => spaced(p)
      case '\n' => spaced(p)
      case c => spaced(p << c)
    }

  describe("Retroactively, allow spaces in arbitrary positions") {

    val sp = spaced(parens)

    sp shouldParse "((()))"
    sp shouldParse "((( )))"
    sp shouldParse "( (( )))"
    sp shouldParse "( (( ))) "
    sp shouldParse "( (\n    (\n )) ) "
    sp shouldNotParse "( (    ( )) "
  }

  describe("Allowing parens in code blocks") {
    // example:
    // aaaaa
    // ~~~
    // (
    // ~~~
    // aa
    // aaaa
    // aa
    //
    val as: Parser[Any] = some(some('a') | many('a') <~ '\n')

    as shouldParse "aaa"
    as shouldParse "aaa\n"
    as shouldParse "\n"
    as shouldParse "aa\naa"

    val marker: Parser[Any] = "\n~~~\n"

    // We have two states: Inside the code block and outside the code block
    def inCode[R, S](text: Parser[R], code: Parser[S]): NT[(R, S)] =
      ( marker ~> inText(text, code)
      | eat { c => inCode(text, code << c) }
      )

    def inText[R, S](text: Parser[R], code: Parser[S]): NT[(R, S)] =
      ( done(text & code)
      | marker ~> inCode(text, code)
      | eat { c => inText(text << c, code) }
      )

    val parensAs = inText(as, parens)

    parensAs shouldParse "a"
    parensAs shouldParse """aaa
                           |~~~
                           |()
                           |~~~
                           |aaaaa
                           |""".stripMargin('|')

    parensAs shouldNotParse """aaa
                              |~~~
                              |(
                              |~~~
                              |aaaaa
                              |""".stripMargin('|')

    parensAs shouldParse """aaa
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

    def un(s: String): String = StringContext treatEscapes s

    def unescape[T](p: Parser[T]): Parser[T] =
      done(p) | eat {
        case '\\' => char >> { c =>
          unescape( p <<< un(s"\\$c") )
        }
        case c => unescape(p << c)
      }

    unescape(many('\n')) shouldParse """\n\n\n"""
    unescape(many("\n" | "a")) shouldParse """\na\n\n"""
    unescape(many("\n" | "a")) shouldParse """\na\n\naaa"""
  }

}
