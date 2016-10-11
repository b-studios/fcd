package fcd

/**
 * Section 4 - Applications
 * ==========================
 * This file contains all code examples from section 5 of our paper:
 *
 *   Brachthäuser, Rendel, Ostermann.
 *   Parsing with First-Class Derivatives
 *   To appear in OOPSLA 2016.
 *
 * Section 4 gives additional applications and use cases where our approach
 * results in a modular solution.
 */
trait Section4 extends ParserUsage { self: Section3 =>

  // Require a library implementation that also supports the derived combinators
  type Parsers <: RichParsers

  // import all symbols from the library
  import parsers._


  /**
   * Section 4.1 - Increased Reuuse through Parser Selection
   */
  object section_4_1 {

    // very simplified grammar to illustrate parser selection
    import section_3_5_improved._

    lazy val stmt: NT[Any] =
      ("while" ~ space ~ "(true):" ~ block
      | some('x') ~ '\n'
      )

    lazy val stmts = many(stmt)
    lazy val block: NT[Any] = '\n' ~ indented(stmts)

    // ### Example: Retroactive selection of the while statement nonterminal
    //
    // For instance, compare:
    //
    //   > whileStmt parse "while (true):\n  xxxxx\n  xxxxx\n"
    //
    //   > whileStmt parse "xxx\n"
    //
    //   > stmt parse "xxx\n"
    lazy val whileStmt = "while" ~> (stmt <<< "while")

    lazy val untilStmt = "until" ~> (stmt <<< "while")
  }

  /**
   * Section 4.2 Modular Definitions as Combinators
   */
  object section_4_2 {
    def unescChar(c: Char): String = StringContext treatEscapes s"\\$c"

    // ### Example. Preprocessor that unescapes backslash escaped characters
    //
    // For instance, try
    //
    //   unescape(many("\n" | "a")) parse "\\na\\n\\naaa"
    def unescape[T](p: Parser[T]): Parser[T] =
      done(p) | eat {
        case '\\' => char >> { c =>
          unescape( p <<< unescChar(c) )
        }
        case c => unescape(p << c)
      }

    val lineEnd = many(' ') ~ '\n'

    // ### Example Figure 6a. Combinators for interleaved parsing of fenced code
    //                        blocks.
    val marker: Parser[Any] = lineEnd ~ "~~~" ~ lineEnd

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

    // Simple variant of balanced parenthesis
    lazy val parens: NT[Any] = '(' ~ parens ~ ')' | succeed(())

    // Blocks of "a"s, such as:
    //
    //   aaaa
    //   a
    //
    //   aaaaa
    //   aaaaa
    val as: Parser[Any] = some(many('a') <~ lineEnd)

    // Now we can retroactively combine the two parsers `parens` and `as` by
    // The resulting parser can parse for instance words like
    //
    //   aaaa
    //   ~~~
    //   ()
    //   ~~~
    //   aaa
    //
    // But more importantly also words such as:
    //
    //   aaa
    //   ~~~
    //   ((())
    //   ~~~
    //   a
    //   ~~~
    //   )
    //   ~~~
    //   a
    //
    // Also see `DerivativeParsersTests.scala` for executing similar examples.
    val both = inText(as, parens)

    // ### Example. (Not shown in the paper) Retroactively allow spaces in
    //              arbitrary positions.
    //
    // We will use this combinator in the following example
    def spaced[T]: Parser[T] => Parser[T] = p =>
      done(p) | eat {
        case ' '  => spaced(p)
        case '\n' => spaced(p)
        case c    => spaced(p << c)
      }

    // ### Example Figure 6c. Modular definition of a parser combinator for
    //                        ASCII-tables.
    // The only parser that makes use of `<<` is delegateCells, all others just
    // use standard combinators

    type Layout = List[Int]

    def table[T](cell: Parser[T]): NT[List[List[T]]] =
      (head <~ lineEnd) >> { layout => body(layout, cell) }

    // a parser computing the table layout
    def head: Parser[Layout] = some('+'~> manyCount('-')) <~ '+'

    def body[T](layout: Layout, cell: Parser[T]): Parser[List[List[T]]] =
      many(rowLine(layout, layout.map(n => cell)) <~ rowSeparator(layout))

    // given a layout, creates a parser for row separators
    def rowSeparator(layout: Layout): Parser[Any] =
      layout.map { n => ("-" * n) + "+" }.foldLeft("+")(_+_) ~ lineEnd

    // either read another rowLine or quit cell parsers and collect results
    def rowLine[T](layout: Layout, cells: List[Parser[T]]): Parser[List[T]] =
      ( ('|' ~> distr(delegateCells(layout, cells)) <~ lineEnd) >> { cs => rowLine(layout, cs) }
      | collect(cells)
      )

    // first feed n tokens to every cell parser, then feed newline and read a pipe
    def delegateCells[T](layout: Layout, cells: List[Parser[T]]): List[Parser[Parser[T]]] =
      layout.zip(cells).map {
        case (n, p) => delegateN(n, p).map(_ << '\n') <~ '|'
      }

    // We can use the table combinator recursively to parse nested tables.
    // See `DerivativeParsersTests.scala` for examples.

    // Here we now finally combine the modularly defined combinators for
    // - mixed content
    // - retroactive spacing
    // - tables
    //
    // This allows parsing of words like
    //
    //   +----+
    //   |aa  |
    //   |~~~ |
    //   |(())|
    //   |~~~ |
    //   |aaaa|
    //   +----+
    lazy val combined: NT[Any]    = inText(asAndTables, spaced(parens))
    lazy val asAndTables: NT[Any] = as | table(combined)

    // Again, some more examples of words that are recognized by `combined` can
    // be found in `DerivativeParsersTests.scala`.
  }

}
