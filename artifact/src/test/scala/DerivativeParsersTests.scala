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

  describe("Biased choice") {
    val p = biasedAlt("foo", some(letter)) ~ "bar"

    p shouldParse "foobar"
    p shouldNotParse "foozbar"
    p shouldParse "barbar"

    // this test shows, that we can only implement a locally biased choice
    val q = biasedAlt("foo", "f") ~ "oo"

    // should actually *not* parse "foo", but does:
    q shouldParse "foo"
  }

  describe("Greedy repitition") {

    it ("should return only the result of the longest match") {
      greedySome(some('a')) parse ""    shouldBe List()
      greedyMany(some('a')) parse ""    shouldBe List(List())
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

    // This shows that our implementation is only locally greedy
    println(greedySome("ab" | "a") ~ "b" parse "abab")
  }

  describe("how to locally rewrite biased choice") {

    // Problem with our biased choice is that it is local.
    // That is, for the parsers (p <| q) ~ r and
    //   p = "foo"
    //   q = "f"
    //   r = "oo"
    //
    // the string "foo" is recognized but it should NOT, since p matches
    // "foo" -- however the input is not "foooo".

    val p: Parser[Any] = "foo"
    val q: Parser[Any] = "f"
    val r: Parser[Any] = ("oo" | "b")

    val ex: Parser[Any] = biasedAlt(p, q) ~ r
    // ex shouldNotParse "foo" //-> fails

    // If the right-hand-side `r` is locally known the parser can be
    // rewritten to:

    val rewrite = p ~ r | (neg(p ~ always) &> (q ~ r))
    rewrite shouldNotParse "foo"
    rewrite shouldParse "foooo"
    rewrite shouldParse "fb"
  }

  // Since "lexing" is performed after indentation checking, but indentation
  // checking requires knowledge about the lexical structure, the line joining
  // combinators perform two tasks:
  //
  // 1. Approximate lexical structure by (partially) "reusing" the definitions of
  //    the lexers
  // 2. Convert joined newlines into special tokens `↩` for indentation
  //    checking and convert them back to '\n' after indentation checking.
  //
  // The line joining indentation checker then is defined by
  //
  //   joiningIndent(p) = ilj(elj(mlj(indented(unmask(p)))))
  //
  // That is, the input to `p` is unmasked again, so `↩` tokens are only
  // used to communicate to the indentation checker that these newlines should
  // be ignored.
  describe("Lexically aware line joining by masking") {

    import scala.collection.mutable

    // regions inside skip will not be treated by f.
    // `region` and `skip` should not have an intersection.
    def transform[T](region: Parser[Any], skip: Parser[Any], f: Parser[Parser[T]] => Parser[Parser[T]]): Parser[T] => Parser[T] = {

      // to prevent accessive re-parsing we introduce some caching on this
      // parser combinator here.
      val cache = mutable.WeakHashMap.empty[Parser[T], Parser[T]]

      def rec: Parser[T] => Parser[T] = p => cache.getOrElseUpdate(p, {

        lazy val dp = delegate(p)
        nonterminal (
          done(p) | biasedAlt(
            ( skip   &> dp
            | region &> f(dp)
            ) >> rec,
          (any &> dp) >> rec))
      })
      rec
    }

    // parsers as input transformers
    def filterNewlines[T] = filter[T](_ != '\n')
    def mask[T]           = mapInPartial[T] { case '\n' => '↩' }
    def toSpace[T]        = mapInPartial[T] { case '\n' => ' ' }
    def unmask[T]         = mapInPartial[T] { case '↩' => '\n' }

    // some lexers
    val singleString: Parser[String] = consumed('"' ~ many(nonOf("\"\n")) ~ '"')
    val comment: Parser[String]      = consumed('#' ~ many(nonOf("\n")) ~ '\n')
    val multilineString: Parser[String] = consumed("'''" ~ neg(always ~ prefix("'''")) ~ "'''")

    singleString shouldParse "\"hello world\""
    singleString shouldNotParse "\"hello\nworld\""
    singleString shouldParse "\"hello'''world\""
    multilineString shouldParse "'''Hello \" \n\" world'''"

    // for testing
    val collect = consumed(always) ^^ { x => x.mkString }

    // for now just filter newlines
    val p = transform[String](multilineString, singleString | comment, filterNewlines)(collect)

    it("should only filter newlines in multiline strings") {
      (p parse "hello '''foo\n\"bar''' test\n foo \" bar'''foo \"\n") should be (List("hello '''foo\"bar''' test\n foo \" bar'''foo \"\n"))
    }
    // here we can already observe performance problems (about 400ms):
    p shouldParse "hello '''foo\n\"bar''' test\n foo \" bar'''foo \"\n some content that is not a program, but could be one \n. # ''' some comment \nIt contains newlines \n, \"and some Strings\". Even Multiline strings with '''newlines\n'''."


    lazy val noText: Parser[Any] = comment | singleString | multilineString

    // While the `transform` combinator can be readily used to implement
    // `multiline` and `elj`, implicit line joining (`ilj`) requires an even
    // more specialized treatment: skip-section might occur inside of the
    // region. For instance the first closing bracket in "(\n # ) \n )" should
    // not count.
    val pairs = Map[Elem, Elem]('(' -> ')', '[' -> ']', '{' -> '}')
    val (opening, closing) = (pairs.keys.toList, pairs.values.toList)


    lazy val dyck: NT[Any] = one(opening) >> { paren => many(dyck) ~ pairs(paren) }
      //'(' ~> many(dyck) <~ ')'

    // within comments and strings filter out everything
    val parens =
      // we need to intersect with the outermost parenthesis to prevent
      // parsing something like "aaa()aaa"
      (one(opening) >> { paren => always ~ pairs(paren) }) &>
        transform[Any](noText | nonOf(opening) & nonOf(closing) , err, skip)(dyck)

    parens shouldParse "()"
    parens shouldParse "(())"
    parens shouldParse "(()()())"
    parens shouldParse "(()[]())"
    parens shouldParse "(()[()[]]())"
    parens shouldNotParse "(()[()[]())"
    parens shouldNotParse "a (()) a"
    parens shouldNotParse "(()"
    parens shouldParse "( hello world ())"
    parens shouldParse "( [# foo \"()) \n ()]{\" [ \" hello } world ())"
    parens shouldNotParse "( [# foo \"()) \n ()]{\" [ \" hello world ())"
    parens shouldNotParse "( [# foo \"()) \n ()]\" [ \" hello } world ())"
    parens shouldNotParse "( [# foo \"()) \n )]{\" [ \" hello } world ())"
    parens shouldParse "( hello \" ) \"world ())"
    parens shouldNotParse "( hello \" ) \""

    lazy val escapedNL = '\\' ~ '\n'

    // multiline string line joining
    def mlj[T] = transform[T](multilineString, comment | singleString, mask)
    // implicit line joining in parenthesis
    def ilj[T] = transform[T](parens, noText, mask)
    // explicit line joining by escape sequences
    def elj[T] = transform[T](escapedNL, noText, mask)

    // reusing some definition of `indented`
    import section_3_5_improved._
    def joiningIndent[T]: Parser[T] => Parser[T] = p =>
      ilj(elj(mlj(indented(unmask(p)))))


    it("should mask perform line joining before checking indentation") {
      (joiningIndent(collect) parse "  foo'''a \n a'''\n  bar\n  ( \n )\n") should be (
        List("foo'''a \n a'''\nbar\n( \n )\n")
      )
      (joiningIndent(collect) parse "  '''some \n multiline \n'''\n  ( # comment (\n ) hello\n  test and \\\n escaped\n") should be (
        List("'''some \n multiline \n'''\n( # comment (\n ) hello\ntest and \\\n escaped\n")
      )
    }
    joiningIndent(collect) shouldParse "  '''some \n multiline \n'''\n  ( # comment (\n )\n"
    joiningIndent(collect) shouldNotParse "  '''some \n multiline \n''\n  ( # comment (\n )\n"


    val WS: Parser[Any] = ' '
    val spacesNoNl = some(WS)
    val NL: Parser[Any] = '\n'
    val spaces = many(WS | NL | comment)
    val lineEnd = opt(spacesNoNl) ~ (NL | comment)

    val id: Parser[String] = consumed(some(letter))

    // Python Parser Skeleton

    lazy val expr: NT[Any] = id | singleString | multilineString | "(" ~> spaces ~> expr <~ spaces <~ ")" | "[" ~> spaces ~> opt(someSep(expr, spaces ~ "," ~ spaces) ~ spaces)  <~ "]"
    lazy val stmt: NT[Any] = expr <~ lineEnd | "def" ~> spacesNoNl ~> id ~ ("():" ~> suite)
    lazy val stmts: NT[Any] = someSep(stmt, spaces)
    lazy val suite: NT[Any] = lineEnd ~> joiningIndent(stmts)

    stmt shouldParse    "def foo():\n  '''hello\n '''\n"
    stmt shouldNotParse "def foo():\n  \"'''hello\n '''\"\n"
    stmt shouldParse    "def foo():\n  '''hello\n ''' # some comment  \n"
    stmt shouldNotParse "def foo():\n  # '''hello\n ''' some comment  \n"
    stmt shouldParse    "def foo():\n  []\n"
    stmt shouldParse    "def foo():\n  [foo, bar]\n"
    stmt shouldParse    "def foo():\n  [foo, \nbar]\n"
    stmt shouldNotParse "def foo():\n  \"[foo, \nbar]\"\n"
    stmt shouldParse    "def foo():\n  \"[foo, bar]\"\n"
    stmt shouldParse    "def foo():\n  foo\n  def bar():\n    \"hello\"\n  bar\n"
    stmt shouldParse    "def foo():\n  foo\n  def bar():\n    '''\nhello\n'''\n  bar\n"
  }

  describe("Regression: `not` should preserve invariant `p.results.isEmpty != p.accepts`") {
    val p = neg("a" | "b")
    val p_a = p <<< "a"
    val p_b = p <<< "b"
    val p_c = p <<< "c"

    it ("should preserve the invariant when performing optimization rewrites") {
      p_a.accepts shouldBe false
      p_a.accepts shouldBe (!p_a.results.isEmpty)
      p_b.accepts shouldBe false
      p_b.accepts shouldBe (!p_b.results.isEmpty)
      p_c.accepts shouldBe true
      p_c.accepts shouldBe (!p_c.results.isEmpty)
    }
  }

}
