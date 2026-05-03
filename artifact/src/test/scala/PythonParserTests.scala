package fcd
package test

import scala.language.implicitConversions
import org.scalatest.funspec.AnyFunSpec

class PythonParserTests
    extends AnyFunSpec
    with CustomMatchers[PythonParsers.type](PythonParsers) {

  import parsers._
  import parsers.given
  import Lexeme._

  describe("indented python parser (lexeme based)") {
    indented(many(many(Id("A")) <~ NL)) shouldParseWith (
      List(WS, WS, Id("A"), Id("A"), NL, WS, WS, Id("A"), NL),
      List(List(Id("A"), Id("A")), List(Id("A")))
    )
  }

  describe("implicit line joining") {
    given kw: Conversion[String, Lexeme] = KW(_)
    given punct: Conversion[Char, Lexeme] = p => Punct(p.toString)

    val p = many(WS | id | '(' | ')' | '[' | ']')
    val a = Id("A")
    val BS = Punct("\\")

    dyck shouldParse List[Lexeme]('(', '(', ')', ')')
    dyck shouldNotParse List[Lexeme]('(', '(', ')')
    extDyck shouldParse List('(', a, '(', a, NL, a, ')', a, ')')
    extDyck shouldNotParse List(a, '(', a, '(', a, NL, a, ')', a, ')', a)

    implicitJoin(p) shouldParse List(a, a, a, a, a)
    implicitJoin(p) shouldNotParse List(a, a, a, NL, a, a)
    implicitJoin(p) shouldParse List(a, a, '(', a, NL, a, ')', a)
    implicitJoin(p) shouldNotParse List(a, a, '(', a, NL, a, a)
    implicitJoin(p) shouldNotParse List(a, a, '(', a, '(', NL, a, ')', a)
    implicitJoin(p) shouldParse List(a, a, '(', a, '(', NL, a, ')', ')', a)
    implicitJoin(p) shouldParse List(a, a, '(', a, '[', NL, a, ']', ')', a)
    implicitJoin(p) shouldNotParse List(a, a, '(', a, '[', NL, a, ')', ']', a)

    explicitJoin(p) shouldParse List(a, a, a, BS, NL, a, a)
    explicitJoin(p) shouldParse List(a, a, a, BS, NL, a, a, BS, NL, a, a)

    val input = List[Lexeme](a, NL, Comment("Hey!!"), a, BS, NL, a, a, '(', a,
      '[', a, BS, NL, a, NL, a, ']', ')', a)

    val inputWithoutComments = List[Lexeme](a, NL, a, BS, NL, a, a, '(', a, '[',
      a, BS, NL, a, NL, a, ']', ')', a)

    val inputWithoutExplicit =
      List[Lexeme](a, NL, a, a, a, '(', a, '[', a, a, NL, a, ']', ')', a)

    val inputResult =
      List[Lexeme](a, NL, a, a, a, '(', a, '[', a, a, a, ']', ')', a)

    val collect = consumed(many(any))

    stripComments(collect) shouldParseWith (input, inputWithoutComments)
    explicitJoin(collect) shouldParseWith
      (inputWithoutComments, inputWithoutExplicit)
    implicitJoin(collect) shouldParseWith (inputWithoutExplicit, inputResult)

    preprocess(file_input) shouldParse List(a, ';', a, '=', "yield", "from", a,
      '=', a, ';', NL, NL, a, ';', a, NL, EOS)

    preprocess(file_input) shouldParse
      List(a, '=', a, Punct(">>"), a, '*', a, NL, EOS)

    val sampleProg =
      List[Lexeme]("def", WS, Id("fun"), '(', WS, a, WS, ')', ':', NL, WS, WS,
        a, Punct("+="), WS, a, NL, WS, WS, a, Punct("*="), a, NL, EOS)

    parse(stripComments(collect), sampleProg) shouldBe List(sampleProg)
    parse(explicitJoin(collect), sampleProg) shouldBe List(sampleProg)
    parse(implicitJoin(collect), sampleProg) shouldBe List(sampleProg)

    preprocess(file_input) shouldParse sampleProg

    val sampleProg2 = List[Lexeme]("def", WS, Id("fun"), '(', NL, WS, a, WS, NL,
      ')', ':', NL, WS, WS, a, Punct("+="), Comment("Test"), BS, NL, WS, a, NL,
      WS, WS, a, Punct("*="), a, NL, EOS)

    parse(preprocess(collect), sampleProg2) shouldBe List(sampleProg)
    preprocess(file_input) shouldParse sampleProg2

    // https://en.wikibooks.org/wiki/Python_Programming/Decorators
    // format: off
    val traceProg = List[Lexeme](
      Comment("define the Trace class that will be "), NL,
      Comment("invoked using decorators"), NL,
      "class", WS, Id("Trace"), '(', Id("object"), ')', ':', NL,
      WS, WS, WS, WS, "def", WS, Id("__init__"), '(', Id("self"), ')', ':', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, Id("self"), '.', Id("f"), WS, '=', WS, Id("f"), NL,
      WS, WS, WS, WS, NL, WS, WS, WS, WS, WS, WS, "def", WS, Id("__call__"), '(', Id("self"), WS, ',', '*', Id("args"), ',', WS, Punct("**"), Id("kwargs"), ')', ':', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), '(', Str("entering function "), WS, '+', WS, Id("self"), '.', Id("f"), '.', Id("__name__"), ')', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), '=', Num("0"), NL,
      WS, WS, WS, WS, WS, WS, WS, WS, "for", WS, Id("arg"), WS, "in", WS, Id("args"), ':', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), '(', Str("arg {0}: {1}"), '.', Id("format"), '(', Id("i"), ',', Id("arg"), ')', ')', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), '=', Id("i"), '+', Num("1"), NL,
      WS, WS, WS, WS, WS, WS, WS, WS, NL, WS, WS, WS, WS, WS, WS, WS, WS, "return", WS, Id("self"), '.', Id("f"), '(', '*', Id("args"), ',', WS, Punct("**"), Id("kwargs"), ')', NL,
      EOS
    )
    // format: on

    argument shouldParse List('*', Id("kwargs"))
    argument shouldParse List(Punct("**"), Id("kwargs"))
    arglist shouldParse List(Punct("**"), Id("kwargs2"))
    arglist shouldParse List(Id("kwargs"), ',', WS, Id("kwargs"))
    arglist shouldParse List('*', Id("kwargs"), ',', '*', Id("kwargs"))
    arglist shouldParse List(Punct("**"), Id("kwargs"), ',', Punct("**"),
      Id("kwargs"))
    arglist shouldParse List('*', Id("kwargs"), ',', WS, '*', Id("kwargs"))
    arglist shouldParse List(Punct("**"), Id("kwargs"), ',', WS, Punct("**"),
      Id("kwargs"))
    arglist shouldParse List('(', Id("args"), ',', WS, Id("kwargs"), ')')
    arglist shouldParse List('(', '*', Id("args"), ',', WS, Id("kwargs"), ')')

    arglist shouldParse
      List('(', '*', Id("args"), ',', WS, '*', Id("kwargs"), ')')

    test shouldParse
      List(Id("f"), '(', Id("args"), ',', WS, Id("kwargs"), ')')

    test shouldParse
      List(Id("f"), '(', '*', Id("args"), ',', WS, Punct("**"), Id("kwargs"),
        ')')

    test shouldParse List(Id("print"), '(', Str("entering function "), WS, '+',
      WS, Id("self"), '.', Id("f"), '.', Id("__name__"), ')')

    // TODO is already ambiguous
    // (stmt `parse` List[Lexeme](Id("self"), '.', Id("f"), WS, '=', WS, Id("f"), NL)).size shouldBe 1

    // preprocess(file_input) shouldParse traceProg

    // (stmt `parse` List[Lexeme](
    //     "for", WS, Id("arg"), WS, "in", WS, Id("args"), ':', NL,
    //     WS, WS, Id("print"), NL)).size shouldBe 1

    // format: off
    stmt shouldNotParse List(
      "def", WS, Id("__call__"), '(', Id("self"), WS, ',', '*', Id("args"), ',', WS, Punct("**"), Id("kwargs"), ')', ':', NL,
      WS, WS, "for", WS, Id("arg"), WS, "in", WS, Id("args"), ':', NL,
      WS, WS, WS, WS, Id("print"), NL, // this line is indented too far
      WS, WS, WS, WS, WS, WS, Id("print"), NL
    )
    // format: on

    // with empty lines
    // format: off
    val traceProg2 = List[Lexeme](
      Comment("define the Trace class that will be "), NL,
      Comment("invoked using decorators"), NL,
      "class", WS, Id("Trace"), '(', Id("object"), ')', ':', NL, WS,
      WS, WS, WS, "def", WS, Id("__init__"), '(', Id("self"), ')', ':', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, Id("self"), '.', Id("f"), WS, '=', WS, Id("f"), NL,
      NL,
      WS, WS, WS, WS, "def", WS, Id("__call__"), '(', Id("self"), WS, ',', '*', Id("args"), ',', WS, Punct("**"), Id("kwargs"), ')', ':', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), '(', Str("entering function "), WS, '+', WS, Id("self"), '.', Id("f"), '.', Id("__name__"),
      ')', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), '=', Num("0"), NL,
      WS, WS, WS, WS, WS, WS, WS, WS, "for", WS, Id("arg"), WS, "in", WS, Id("args"), ':', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), '(', Str("arg {0}: {1}"), '.', Id("format"), '(', Id("i"), ',', Id("arg"), ')', ')', NL,
      WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), '=', Id("i"), '+', Num("1"), NL,
      WS, WS, NL,
      NL,
      NL,
      NL,
      WS, WS, WS, WS, WS, WS, WS, WS, "return", WS, Id("self"), '.', Id("f"), '(', '*', Id("args"), ',', WS, Punct("**"), Id("kwargs"), ')', NL,
      EOS
    )
    // format: off

    preprocess(file_input) shouldParse traceProg2
    parse(preprocess(file_input), traceProg2).size shouldBe 1

    // suite should `parse` this:
    // format: off
    val dummyin = List(
      NL,
      WS, "def", WS, Id("f"), '(', ')', ':', NL,
      WS, WS, "def", WS, Id("f"), '(', ')', ':', NL,
      WS, WS, WS, Id("print"), NL,
      WS, WS, WS, Id("print"), NL,
      WS, WS, WS, Id("i"), NL
    )
    // format: on

    // println((suite `parse` dummyin) mkString "\n\n")

    stmt shouldNotParse List(WS, WS, WS, Id("i"), NL)
    atom shouldNotParse List(WS, WS, WS, Id("i"))

    // This is the skeleton of the python parsers (and it is unambiguous)
    lazy val aStmt: NT[Any] = aSimpleStmt | "def" ~> aBlock
    lazy val aSimpleStmt = a <~ NL
    lazy val aBlock =
      aSimpleStmt | NL ~> indented(some(many(emptyLine) ~> aStmt))
    lazy val aInput: NT[Any] = NL.* ~> many(aStmt <~ NL.*) <~ EOS

    // format: off
    val dummyin2 = List[Lexeme](
      "def", NL,
      WS, a, NL,
      WS, a, NL,
      WS, "def", NL,
      WS, WS, a, NL,
      WS, WS, a, NL,
      WS, WS, a, NL,
      NL, "def", NL,
      WS, a, NL,
      WS, a, NL,
      WS, "def", NL,
      WS, WS, WS, WS, WS, WS, a, NL,
      WS, WS, WS, WS, WS, WS, a, NL,
      WS, WS, WS, WS, WS, WS, a, NL,
      EOS
    )
    // format: on

    aInput shouldParse List("def", NL, WS, WS, a, NL, WS, WS, a, NL, EOS)
    aInput shouldNotParse List("def", NL, WS, WS, a, NL, WS, a, NL, EOS)
    aInput shouldParse List("def", NL, WS, WS, a, NL, NL, WS, WS, a, NL, EOS)
    aInput shouldNotParse List("def", NL, WS, WS, a, NL, NL, WS, a, NL, EOS)

    indentBy(WS ~ WS)(collect) shouldParseWith
      (List(WS, WS, a, NL), List(a, NL))

    indentBy(WS ~ WS)(collect) shouldParseWith
      (List(WS, WS, NL, NL, WS, WS, a, NL), List(NL, NL, a, NL))

    parse(aInput, dummyin2).size shouldBe 1
  }
}
