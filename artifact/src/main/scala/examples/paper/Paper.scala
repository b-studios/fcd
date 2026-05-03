package fcd

/** This object instantiates the examples from section 3, 4 and 7 and makes them
  * available in the REPL via:
  *
  * > import paper._
  */

object paper
    extends RichParsers
    with DerivativeParsers
    with Section3
    with Section4
    with Section7
