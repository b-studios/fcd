package fcd
package test
package performance

import org.scalameter.api._

object SimplePerformanceTest extends Bench.OnlineRegressionReport  {




  val baseTable = """+---+
                    ^|xxx|
                    ^+---+
                    ^""".stripMargin('^')

  def width(str: String): Int = str.lines.next.size
  def height(str: String): Int = str.lines.size
  def doubleRule(width: Int): String = s"""+${"-" * width}+${"-" * width}+"""
  def vertBar(height: Int): String   = "|" * height

  def nest2x2(table: String): String = {
    val w = width(table)
    ( doubleRule(w) + "\n" + table.lines.toList.map(l => s"|$l|$l|").mkString("\n") + "\n" +
      doubleRule(w) + "\n" + table.lines.toList.map(l => s"|$l|$l|").mkString("\n") + "\n" +
      doubleRule(w) + "\n")
  }

  val tableNesting = Gen.range("table nesting depth")(0, 3, 1)

  val sampleTables = tableNesting.map { d =>
    (0 until d).foldLeft(baseTable) { case (t, _) => nest2x2(t) }
  }

  object tableParsers {

    val parsers: DerivativeParsers.type = DerivativeParsers
    import parsers._

    // copied here from DerivativeParsersTests, but should be extracted into some module.
    type Layout = List[Int]

    // A parser computing the table layout
    lazy val head: Parser[Layout] = some('+'~> manyCount('-')) <~ '+' <~ '\n'

    def zipWith[A,B](l1: List[A => B], l2: List[A]): List[B] =
      (l1 zip l2).map { case (f, x) => f(x) }

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
    lazy val nestedTables: NT[Any] = table(xs | nestedTables)
  }

  performance of "table" in {

    import tableParsers._
    import parsers._

    measure method "parse" in {
      using(sampleTables) in { str =>
        resetCache()
        val List(res) = parse(nestedTables, str)
      }
    }
  }

}
