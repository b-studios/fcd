package fcd
package test

import org.scalatest._
import scala.language.implicitConversions

class PythonParserTests extends FunSpec with Matchers {

  val parsers = PythonParsers
  import parsers._

  // TODO currently just inlined. Fix modularity of this implementation!
  implicit class ParserTests[T, P <% Parser[T]](p: => P) {
    def shouldParse[ES <% Iterable[Elem]](s: ES, tags: Tag*) =
      it (s"""should parse "$s" """, tags:_*) {
        accepts(p, s) shouldBe true
      }
    def shouldNotParse[ES <% Iterable[Elem]](s: ES, tags: Tag*) =
      it (s"""should not parse "$s" """, tags:_*) {
        accepts(p, s) shouldBe false
      }
    // for unambiguous parses
    def shouldParseWith[ES <% Iterable[Elem]](s: ES, result: T) =
      it (s"""should parse "$s" with correct result""") {
        parse(p, s) shouldBe List(result)
      }
  }

  describe ("indented python parser (lexeme based)") {
    indented(many(many(Id("A")) <~ NL)) shouldParseWith (
      List(WS, WS, Id("A"), Id("A"), NL,
           WS, WS, Id("A"), NL),
      List(List(Id("A"), Id("A")), List(Id("A"))))
  }

  describe ("implicit line joining") {

    implicit def keyword(kw: Symbol): Lexeme = KW(kw.name)
    implicit def punctuation(p: String): Lexeme = Punct(p)

    val p     = many(WS | id | "(" | ")" | "[" | "]")
    val a     = Id("A")
    val BS    = Punct("\\")


    dyck    shouldParse    List[Lexeme]("(", "(", ")", ")")
    dyck    shouldNotParse List[Lexeme]("(", "(", ")")
    extDyck shouldParse    List[Lexeme](a, "(", a, "(", a, NL, a, ")", a, ")")

    implicitJoin(p) shouldParse     List[Lexeme](a, a, a, a, a)
    implicitJoin(p) shouldNotParse  List[Lexeme](a, a, a, NL, a, a)
    implicitJoin(p) shouldParse     List[Lexeme](a, a, "(", a, NL, a, ")", a)
    implicitJoin(p) shouldNotParse  List[Lexeme](a, a, "(", a, NL, a, a)
    implicitJoin(p) shouldNotParse  List[Lexeme](a, a, "(", a, "(",  NL, a, ")", a)
    implicitJoin(p) shouldParse     List[Lexeme](a, a, "(", a, "(",  NL, a, ")", ")", a)
    implicitJoin(p) shouldParse     List[Lexeme](a, a, "(", a, "[",  NL, a, "]", ")", a)
    implicitJoin(p) shouldNotParse  List[Lexeme](a, a, "(", a, "[",  NL, a, ")", "]", a)

    explicitJoin(p) shouldParse     List[Lexeme](a, a, a, BS, NL, a, a)
    explicitJoin(p) shouldParse     List[Lexeme](a, a, a, BS, NL, a, a, BS, NL, a, a)

    val input = List[Lexeme](
      a, NL,
      Comment("Hey!!"), a, BS, NL,
      a, a, "(", a, "[", a, BS, NL,
      a, NL,
      a, "]", ")", a)

    val inputWithoutComments = List[Lexeme](
      a, NL,
      a, BS, NL,
      a, a, "(", a, "[", a, BS, NL,
      a, NL,
      a, "]", ")", a)

    val inputWithoutExplicit = List[Lexeme](
      a, NL,
      a,
      a, a, "(", a, "[", a,
      a, NL,
      a, "]", ")", a)

    val inputResult = List[Lexeme](
      a, NL,
      a,
      a, a, "(", a, "[", a,
      a,
      a, "]", ")", a)

    val collect = consumed(many(any))

    stripComments(collect) shouldParseWith  (input,                inputWithoutComments)
    explicitJoin(collect)  shouldParseWith  (inputWithoutComments, inputWithoutExplicit)
    implicitJoin(collect)  shouldParseWith  (inputWithoutExplicit, inputResult)

    preprocess(file_input) shouldParse List[Lexeme](
      a, ";", a, "=", 'yield, 'from, a, "=", a, ";", NL,
      NL,
      a, ";", a, NL,
      EOS)

    preprocess(file_input) shouldParse List[Lexeme](
      a, "=", a, ">>", a, "*", a, NL,
      EOS)

    val sampleProg = List[Lexeme](
      'def, WS, Id("fun"), "(", WS, a, WS, ")", ":", NL,
      WS, WS, a, "+=", WS, a, NL,
      WS, WS, a, "*=", a, NL,
      EOS)

    (stripComments(collect) parse sampleProg) shouldBe List(sampleProg)
    (explicitJoin(collect) parse sampleProg) shouldBe List(sampleProg)
    (implicitJoin(collect) parse sampleProg) shouldBe List(sampleProg)

    preprocess(file_input) shouldParse sampleProg

    val sampleProg2 = List[Lexeme](
      'def, WS, Id("fun"), "(", NL,
      WS, a, WS,
      NL, ")", ":", NL,
      WS, WS, a, "+=", Comment("Test"), BS, NL,
      WS, a, NL,
      WS, WS, a, "*=", a, NL,
      EOS)

    (preprocess(collect) parse sampleProg2) shouldBe List(sampleProg)

    preprocess(file_input) shouldParse sampleProg2

    // https://en.wikibooks.org/wiki/Python_Programming/Decorators
    val traceProg = List[Lexeme](
        Comment("define the Trace class that will be "), NL,
        Comment("invoked using decorators"), NL,
        'class, WS, Id("Trace"), "(", Id("object"), ")", ":", NL,
        WS, WS, WS, WS, 'def, WS, Id("__init__"), "(", Id("self"), ")", ":", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, Id("self"), ".", Id("f"), WS, "=", WS, Id("f"), NL,
        WS, WS, WS, WS, WS, WS, NL,
        WS, WS, WS, WS, WS, WS, 'def, WS, Id("__call__"), "(", Id("self"), WS, ",", "*", Id("args"), ",", WS, "**", Id("kwargs"), ")", ":", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), "(", Str("entering function "), WS, "+", WS, Id("self"), ".",  Id("f"), ".", Id("__name__"), ")", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), "=", Num("0"), NL,
        WS, WS, WS, WS, WS, WS, WS, WS, 'for, WS, Id("arg"), WS, 'in, WS, Id("args"), ":", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), "(", Str("arg {0}: {1}"), ".", Id("format"), "(", Id("i"), ",", Id("arg"), ")", ")", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), "=", Id("i"), "+", Num("1"), NL,
        WS, WS, WS, WS, WS, WS, WS, WS, NL,
        WS, WS, WS, WS, WS, WS, WS, WS, 'return, WS, Id("self"), ".", Id("f"), "(", "*", Id("args"), ",", WS, "**", Id("kwargs"), ")", NL,
        EOS
    )

    argument shouldParse List[Lexeme]("*", Id("kwargs"))
    argument shouldParse List[Lexeme]("**", Id("kwargs"))
    arglist  shouldParse List[Lexeme]("**", Id("kwargs2"))
    arglist  shouldParse List[Lexeme](Id("kwargs"), ",", WS, Id("kwargs"))
    arglist  shouldParse List[Lexeme]("*", Id("kwargs"), ",", "*", Id("kwargs"))
    arglist  shouldParse List[Lexeme]("**", Id("kwargs"), ",", "**", Id("kwargs"))
    arglist  shouldParse List[Lexeme]("*", Id("kwargs"), ",", WS, "*", Id("kwargs"))
    arglist  shouldParse List[Lexeme]("**", Id("kwargs"), ",", WS, "**", Id("kwargs"))

    arglist shouldParse List[Lexeme]("(", Id("args"), ",", WS, Id("kwargs"), ")")
    arglist shouldParse List[Lexeme]("(", "*", Id("args"), ",", WS, Id("kwargs"), ")")
    arglist shouldParse List[Lexeme]("(", "*", Id("args"), ",", WS, "*", Id("kwargs"), ")")
    test shouldParse List[Lexeme](Id("f"), "(", Id("args"), ",", WS, Id("kwargs"), ")")
    test shouldParse List[Lexeme](Id("f"), "(", "*", Id("args"), ",", WS, "**", Id("kwargs"), ")")

    preprocess(file_input) shouldParse traceProg

    // with empty lines
    val traceProg2 = List[Lexeme](
        Comment("define the Trace class that will be "), NL,
        Comment("invoked using decorators"), NL,
        'class, WS, Id("Trace"), "(", Id("object"), ")", ":", NL,
        WS, WS, WS, WS, 'def, WS, Id("__init__"), "(", Id("self"), ")", ":", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, Id("self"), ".", Id("f"), WS, "=", WS, Id("f"), NL,
        NL,
        WS, WS, WS, WS, WS, WS, 'def, WS, Id("__call__"), "(", Id("self"), WS, ",", "*", Id("args"), ",", WS, "**", Id("kwargs"), ")", ":", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), "(", Str("entering function "), WS, "+", WS, Id("self"), ".",  Id("f"), ".", Id("__name__"), ")", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), "=", Num("0"), NL,
        WS, WS, WS, WS, WS, WS, WS, WS, 'for, WS, Id("arg"), WS, 'in, WS, Id("args"), ":", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), "(", Str("arg {0}: {1}"), ".", Id("format"), "(", Id("i"), ",", Id("arg"), ")", ")", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), "=", Id("i"), "+", Num("1"), NL,
        WS, WS, NL,
        NL,
        WS, WS, WS, WS, WS, WS, WS, WS, 'return, WS, Id("self"), ".", Id("f"), "(", "*", Id("args"), ",", WS, "**", Id("kwargs"), ")", NL,
        EOS
    )

    preprocess(file_input) shouldParse traceProg2

  }
}
