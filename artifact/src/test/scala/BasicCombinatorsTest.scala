package fcd
package test

import org.scalatest._

trait BasicCombinatorTests extends CustomMatchers { self: FunSpec with Matchers =>

  import parsers._

  describe("parser \"abc\"") {
    val p = 'a' ~ 'b' ~ 'c'

    p shouldParse "abc"
    p shouldNotParse "abcd"
  }

  describe("parser \"ab | ac\"") {
    val p = ('a' ~ 'b') | ('a' ~ 'c')
    p shouldParse "ab"
    p shouldParse "ac"
    p shouldNotParse "bc"
    p shouldNotParse "a"
    p shouldNotParse "abc"
  }

  describe("parser \"baaa | ba\"") {
    val p: Parser[_] = ('b' ~ 'a' ~ 'a' ~ 'a') | 'b' ~ 'a'
    p shouldParse "baaa"
    p shouldParse "ba"
    ((p ~ 'c' ~ 'o') | (p ~ 'c')) shouldParse "bac"
    ((p ~ 'c' ~ 'o') | (p ~ 'c')) shouldParse "baco"
  }

  describe("parser \"(baaa | ba) aa\"") {
    val p: Parser[_] = ("baaa" | "ba") ~ "aa"
    p shouldParse "baaaaa"
    p shouldParse "baaa"
  }

  describe("parser \"succeed(a) b\"") {
    val p = succeed('a') ~ 'b'
    p shouldParse "b"
    p shouldNotParse ""
  }

  describe("parser \"succeed(a) succeed(b)\"") {
    val p = succeed('a') ~ succeed('b')
    p shouldParse ""
  }

  describe("parser \"succeed(a) | succeed(b)\"") {
    val p = succeed('a') | succeed('b')
    p shouldParse ""
  }

  describe("parser \"(a a a | a a)+") {
    val p: Parser[_] = 'a' ~ 'a' ~ 'a' | 'a' ~ 'a'
    describe("some(_)") { some(p) shouldParse "aaaa" }
    describe("_ ~ 'b'") { (p ~ 'b') shouldParse "aaab" }
    describe("some(_) ~ 'b'") {
      (some(p) ~ 'b') shouldParse "aab"
      (some(p) ~ 'b') shouldParse "aaab"
      (some(p) ~ 'b') shouldParse "aaaaab"
    }
    describe("some(_ ~ 'a') ~ 'b'") {
      (some(p ~ 'a') ~ 'b') shouldParse "aaaab"
      (some(p ~ 'a') ~ 'b') shouldParse "aaab"
    }
  }

  describe("parser \"'a'+\"") {
    val p = some('a')

    val largeInput = "a" * 100

    p shouldParse "a"
    p shouldParse "aaaaaa"
    p shouldParse largeInput
    p shouldNotParse ""
    p shouldNotParse ("b" + largeInput)
    p shouldNotParse (largeInput + "b")
  }

}
