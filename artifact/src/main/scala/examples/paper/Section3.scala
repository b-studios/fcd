package fcd

/**
 * Section 3 - First-class Derivatives: Gaining
 * Fine Grained Control over the Input Stream
 * ===========================================
 * This file contains all code examples from section 3 of our paper:
 *
 *   Brachthäuser, Rendel, Ostermann.
 *   Parsing with First-Class Derivatives
 *   Submitted to OOPSLA 2016.
 *
 * The examples are grouped by subsections. For every subsection with
 * examples we introduced a corresponding Scala object below.
 *
 * You can experiment with the examples of this file in the REPL by:
 *
 *   > console
 *   scala> import paper.section_3_2._
 *   scala> number.parse("42")
 *   res0: Results[Int] = List(42)
 *
 * You can reach the Scala console by entering 'console' at the
 * sbt prompt.
 *
 * Additional note: All examples are parametrized by the parser combinator
 * library to allow experimenting with different implementations. This should
 * also support future research and alternate implementations.
 *
 * All the traits containing paper examples are eventually combined and
 * instantiated to an object `paper` in `Paper.scala`.
 */

trait Section3 extends ParserUsage {

  // Require a library implementation that also supports the derived combinators
  type Parsers <: RichParsers

  // import all symbols from the library
  import parsers._

  /**
   * Section 3.2 First-Class Derivatives
   */
  object section_3_2 {

    // ### Example of Subsection 3.2: First-Class Derivatives (<<)
    //
    // Difference: The type `P` of parsers in the paper is called `Parser` in
    //             the implementation.
    //
    // To get an intuition for first-class derivatives we encourage you to
    // try the following in the REPL:
    //
    //   > import paper.section_3_2._
    //   > p
    //
    //   > p << 'f'
    //
    //   > p << 'f' << 'o' << 'r'
    //
    // or alternatively to the last command
    //
    //   > p <<< "for"
    //
    // to see the parser fail, try for instance:
    //
    //   > p <<< "foa"
    //
    val p: Parser[String] = "for"

    // ### Example of Subsection 3.2: Combinator "done"
    //
    // To get an intuition for the combinator "done" we encourage you to
    // try the following in the REPL:
    //
    // > import paper.section_3_2._
    // > q
    //
    // > q <<< "aaa"
    //
    // > done(q <<< "aaa")
    //
    // > done(q <<< "aaa") << 'a'
    //
    // If you are interested in a graphical representation of a parser you
    // can print the graph to a file by calling `q.printToFile("filename.png")`.
    //
    // This requires that you have graphviz installed on your computer.
    val q: Parser[List[Char]] = many('a')


    // ### Example of Subsection 3.2: Combinator "nt"
    //
    // Difference: The combinator `nt` in the paper is called `nonterminal` in
    //             our Scala implementation.
    //
    // Wrapping productions with the `nonterminal(...)` combinator can often be
    // inferred by annotating a different type.
    //
    // Try removing `nonterminal` from the implementation of `number`.
    //
    // When attempting to evaluate `number` you will observe a stack overflow,
    // which is a common pitfall.
    //
    // Then try to change `number: Parser[Int]` to `number: NT[Int]`.
    //
    // The implicit conversions that wrap the production into `nonterminal`
    // calls are defined in the file Syntax.scala
    val digit: Parser[Int] = acceptIf(_.isDigit) ^^ { s => Integer.valueOf(s.toString) }

    lazy val number: Parser[Int] =
      nonterminal( number ~ digit ^^ { case (n, d) => (n * 10) + d }
                 | digit
                 )

    // To get an overview of the available parser combinator refer to:
    //
    //   Parsers.scala    - native parser combinators (The interface of parsers
    //                      as found in Figure 1a.)
    //   DerivedOps.scala - derived parser combinators
    //   Syntax.scala     - syntactic sugar for native and derived combinators

  }

  /**
   * Section 3.4 Implementation using First-Class Derivatives
   */
  object section_3_4 {

    // Figure 4a. Definition of the combinator indented(p) in terms of <<.
    def indented[T](p: Parser[T]): Parser[T] =
      done(p) | (space ~ space) ~> readLine(p)

    def readLine[T](p: Parser[T]): Parser[T] =
      ( no('\n')     >> { c => readLine(p << c) }
      | accept('\n') >> { c => indented(p << c) }
      )

    // To inspect the virtual input stream of some parser `p` in `indented(p)`
    // one can use the following parser as kind of "mock-parser"
    //
    // It will accept all words and return the input stream it has processed.
    val collect = consumed(many(any)) map (_.mkString)

    // for instance, you can try the following in the REPL
    //
    //   > indented(collect) parse "  hello\n  world\n"
  }

  /* The example in the paper is quite restrictive: It only models indentation
   * by fixed size (size = 2 in our example).
   * Here we show, how variable indentation can be recognized by reading
   * the level of indentation of the first line and taking this as a measure
   * for the following lines.
   */
  object section_3_4_improved {

    // please note the use of combinator `manyN(n, space)` which recognizes
    // n-many spaces.
    def indentBy[T](n: Int): Parser[T] => Parser[T] = p =>
      done(p) | manyN(n, space) ~> readLine(n)(p)

    // Only change: pass the level of indentation as parameter around
    def readLine[T](n: Int)(p: Parser[T]): Parser[T] =
      ( no('\n')     >> { c => readLine(n)(p << c) }
      | accept('\n') >> { c => indentBy(n)(p << c) }
      )

    // Here we first read some spaces (at least one) and then invoke
    // `indentBy`.
    def indented[T](p: Parser[T]): Parser[T] = consumed(some(space)) >> { case s =>
      // this simulates lookahead for greedy matching
      no(' ') >> { c => indentBy(s.size)(p) <<< s << c }
    }
  }


  /**
   * Derived Combinators
   */
  object section_3_5 {

    // Section 3.5 introduces `delegate` and `repeat`. The implementation of
    // the two combinators can be found in `DerivedOps.scala`.

    // ### Example from section 3.5
    // Parser combinator that given a parser p recognizes two tokens with `p`
    // then recognizes the token 'a' and continues with the rest of `p`.
    // Here `any ~ any` delimits the scope of delegation to `p`
    //
    // For instance you can try
    //
    //   > import paper.section_3_5._
    //   > injectA("for") parse "foar"
    //
    //   > injectA("for") parse "for"
    def injectA[T](p: Parser[T]): Parser[T] =
      ((any ~ any) &> delegate(p)) >> { p2 => 'a' ~> p2 }


    // Not in the paper: Example for usage of combinator `repeat`.
    // every two tokens recognize an intermediate token 'a'.
    //
    // For instance you can try
    //
    //   > injectAs("hello!") parse "heallao!a"
    //
    // Please note, that since we repeatedly delimit with `any ~ any` the
    // resulting parser can only recognize words in { (xxa)* | x ∈ Σ }
    def injectAs[T] = repeat[T] { p =>
        ((any ~ any) &> delegate(p)) <~ 'a'
    }

    // Figure 5b. Definition of the combinator `indented(p)` in terms of `delegate`.
    lazy val line = many(no('\n')) <~ '\n'
    def indented[T]: Parser[T] => Parser[T] = repeat[T] { p =>
      (space ~ space)  ~> (line &> delegate(p))
    }

    // To experiment with this implementation of indented you can selectively
    // import `collect` from `section_3_4` in the REPL or here like:
    import section_3_4.collect

    // Also see `test/scala/DerivativeParsersTests.scala` for more tests
    // involving the indentation combinator.
  }


  /**
   * Symmetrical to section_3_4 and section_3_4_improved we can define flexible
   * indentation using delegate and repeat.
   */
  object section_3_5_improved {

    lazy val line = many(no('\n')) <~ '\n'
    def indentBy[T](n: Int): Parser[T] => Parser[T] = repeat[T] { p =>
      manyN(n, space) ~> (line &> delegate(p))
    }

    def indented[T](p: Parser[T]): Parser[T] = consumed(some(space)) >> { case s =>
      no(' ') >> { c => indentBy(s.size)(p) <<< s << c }
    }
  }
}
