package fcd
package test

import org.scalatest._

trait NegationTests extends CustomMatchers { self: FunSpec with Matchers =>

  import parsers._
  import parsers.{ not => neg }

  describe("parser \"not(aa)\"") {
    val p = neg("aa")
    p shouldParse "a"
    p shouldNotParse "aa"
    p shouldParse "aac"
    p shouldParse "abc"
  }

  describe("parser \"not(aa) & lower*\"") {
    val p = neg("aa") & many(lower)
    p shouldParse "a"
    p shouldParse "bc"
    p shouldParse "ab"
    p shouldNotParse "aa"
    p shouldParse "abc"
    p shouldParse "aac"
    p shouldParse "aacdd"
  }

  describe("parser \"not(aa ~ .*) & lower*\"") {
    val p = neg("aa" ~ many(any)) & many(lower)
    p shouldParse "a"
    p shouldParse "bc"
    p shouldParse "ab"
    p shouldNotParse "aa"
    p shouldParse "abc"
    p shouldNotParse "aac"
    p shouldNotParse "aacadasdasdasd"
  }

  describe("parser \"not(.* ~ abc ~ .*)\"") {
    val p = neg(many(any) ~ "abc" ~ many(any))
    p shouldParse ""
    p shouldParse "xx"
    p shouldParse "xxabxx"
    p shouldNotParse "xxabcxxx"
    p shouldNotParse "xxabc"
    p shouldNotParse "abcxxx"
  }

  describe("parser \"not((baaa | ba) ~ aa ~ .*) & lower*\"") {
    val p: Parser[_] = neg(("baaa" | "ba") ~ "aa" ~ many(any)) & many(lower)
    p shouldNotParse "baaa"
    p shouldNotParse "baaaxx"
    p shouldParse ""
    p shouldParse "baba"
    p shouldParse "baacxx"
  }
}
