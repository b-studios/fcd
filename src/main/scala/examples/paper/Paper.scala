package fcd
object paper extends Section3 with Section4 with Section5 {

  // Use the derivative based parsers for examples in the paper
  type Parsers = DerivativeParsers.type
  def _parsers: DerivativeParsers.type = DerivativeParsers
  override lazy val parsers: DerivativeParsers.type = _parsers

}
