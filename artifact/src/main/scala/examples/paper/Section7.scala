package fcd

/**
 * Section 7 - Implementation
 * ==========================
 * This file contains all code examples from section 7 of our paper:
 *
 *   Brachthäuser, Rendel, Ostermann.
 *   Parsing with First-Class Derivatives
 *   To appear in OOPSLA 2016.
 *
 * Section 7 introduces the implementation of our parser combinator library. In
 * addition to repeating the few examples from the paper in this file we explain
 * the relation between the implementation in the paper and in the artifact.
 *
 * As described in the paper, the core of the implementation builds on
 * derivative based parsing as described by Matt Might et al, translated to an
 * object oriented setting.
 */
trait Section7 extends ParserUsage {

  // Require a library implementation that also supports the derived combinators
  type Parsers <: RichParsers

  // import all symbols from the library
  import parsers._

  /**
   * Section 7.1, introduces the concrete type of a parser as
   *
   *   trait P[+R] {
   *     def results: Res[R]
   *     def derive: Elem => P[R]
   *   }
   *
   * The corresponding concrete type of this artifact can be found in
   * `DerivativeParsers.scala` (corresponding to Figure 10) which contains the
   * implementation of the interface defined in `Parsers.scala`
   * (corresponding to Figure 1a.).
   *
   * Please note the following important differences:
   * - `derive` is called `consume` in this artifact.
   * - the trait `Parser[+R]` has default implementations for the various
   *   combinators. This corresponds to the later developments in Section 7.4
   *   "Compaction by Dynamic Dispatch".
   * - Instead of anonymous subclasses (such as `def fail[R] = new P[R] {...}`)
   *   the various combinators are implemented by named classes / objects
   *   (that is, `object Fail extends P[Nothing] { ... }`).
   * - We added a special primitive parser `always` which is bisimilar to
   *   `many(any)` and thus dual (in some sense) to `fail`. Having it as a
   *   primitive gives rise to some optimizations.
   */
  object section_7 {

    // ### Example. Derivative of some(a)
    //
    // You can print the derivative of `as` by 'a' with
    //
    //    > (as << 'a').printToFile("as_derive_a.png")
    val as = some('a')


    // ### Example. Derivative with compaction
    //
    // You can observe the result of derivation an compaction by comparing
    //
    //   > ex
    //
    // and
    //
    //   > ex << 'a'
    val ex = 'a' ~ (succeed(()) | 'b')

    // The implementation of `nt` corresponding to Figure 7 can be found in
    // class `Nonterminal` in file `DerivativeParsers.scala`. There, also the
    // special treatment to handle exotic terms as discussed in Section 4.3
    // becomes visible.

    // ### Example for an exotic parser in Section 7.3 - Nonterminals.
    // Please note that we use `nonterminal` instead of the paper's abbreviation
    // `nt`.
    lazy val exotic: Parser[Any] = nonterminal(exotic << 'a')

    // We introduce the nonterminal parser combinator to allow for left-recursive
    // grammars. Thus, it might be instructive to also inspect the tests in
    // `test/scala/LeftrecTests.scala`.
  }

}
