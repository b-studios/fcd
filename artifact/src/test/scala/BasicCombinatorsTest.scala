package fcd
package test

import scala.language.implicitConversions
import org.scalatest.funspec.AnyFunSpec

trait BasicCombinatorTests {
  self: AnyFunSpec & CustomMatchers[RichParsers] =>

  import parsers.{succeed as succ, *}

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
    val p = ('b' ~ 'a' ~ 'a' ~ 'a') | 'b' ~ 'a'
    p shouldParse "baaa"
    p shouldParse "ba"
    ((p ~ 'c' ~ 'o') | (p ~ 'c')) shouldParse "bac"
    ((p ~ 'c' ~ 'o') | (p ~ 'c')) shouldParse "baco"
  }

  describe("parser \"(baaa | ba) aa\"") {
    val p = ("baaa" | "ba") ~ "aa"
    p shouldParse "baaaaa"
    p shouldParse "baaa"
  }

  describe("parser \"succeed(a) b\"") {
    val p = succ('a') ~ 'b'
    p shouldParse "b"
    p shouldNotParse ""
  }

  describe("parser \"succeed(a) succeed(b)\"") {
    val p = succ('a') ~ succ('b')
    p shouldParse ""
  }

  describe("parser \"succeed(a) | succeed(b)\"") {
    val p = succ('a') | succ('b')
    p shouldParse ""
  }

  describe("parser \"(a a a | a a)+") {
    val p = 'a' ~ 'a' ~ 'a' | 'a' ~ 'a'
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

    val largeInput = List.fill(100)('a').mkString

    p shouldParse "a"
    p shouldParse "aaaaaa"
    p shouldParse largeInput
    p shouldNotParse ""
    p shouldNotParse "b" + largeInput
    p shouldNotParse largeInput + "b"
  }

}
