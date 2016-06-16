package fcd

/**
 * This object instantiates the examples from section3-5 and makes them
 * available in the REPL via:
 *
 * > import paper._
 */
object paper extends Section3 with Section4 with Section5 {

  // Use the derivative based parsers for examples in the paper
  type Parsers = DerivativeParsers.type
  def _parsers: DerivativeParsers.type = DerivativeParsers
  override lazy val parsers: DerivativeParsers.type = _parsers

}
