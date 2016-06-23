package fcd
package test

import org.scalatest._
import scala.language.implicitConversions

class PythonParserTests extends FunSpec with Matchers {

  val parsers = PythonParsers
  import parsers._

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
        WS, WS, WS, WS, NL,
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

    test shouldParse List[Lexeme](Id("print"), "(", Str("entering function "), WS, "+", WS, Id("self"), ".",  Id("f"), ".", Id("__name__"), ")")

    // TODO is already ambiguous
    // (stmt parse List[Lexeme](Id("self"), ".", Id("f"), WS, "=", WS, Id("f"), NL)).size shouldBe 1

    // preprocess(file_input) shouldParse traceProg

    // (stmt parse List[Lexeme](
    //     'for, WS, Id("arg"), WS, 'in, WS, Id("args"), ":", NL,
    //     WS, WS, Id("print"), NL)).size shouldBe 1

    stmt shouldNotParse List[Lexeme](
        'def, WS, Id("__call__"), "(", Id("self"), WS, ",", "*", Id("args"), ",", WS, "**", Id("kwargs"), ")", ":", NL,
        WS, WS, 'for, WS, Id("arg"), WS, 'in, WS, Id("args"), ":", NL,
        WS, WS, WS, WS, Id("print"), NL,
        // this line is indented too far
        WS, WS, WS, WS, WS, WS, Id("print"), NL)

    // with empty lines
    val traceProg2 = List[Lexeme](
        Comment("define the Trace class that will be "), NL,
        Comment("invoked using decorators"), NL,
        'class, WS, Id("Trace"), "(", Id("object"), ")", ":", NL,
        WS, WS, WS, WS, 'def, WS, Id("__init__"), "(", Id("self"), ")", ":", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, Id("self"), ".", Id("f"), WS, "=", WS, Id("f"), NL,
        NL,
        WS, WS, WS, WS, 'def, WS, Id("__call__"), "(", Id("self"), WS, ",", "*", Id("args"), ",", WS, "**", Id("kwargs"), ")", ":", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), "(", Str("entering function "), WS, "+", WS, Id("self"), ".",  Id("f"), ".", Id("__name__"), ")", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), "=", Num("0"), NL,
        WS, WS, WS, WS, WS, WS, WS, WS, 'for, WS, Id("arg"), WS, 'in, WS, Id("args"), ":", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("print"), "(", Str("arg {0}: {1}"), ".", Id("format"), "(", Id("i"), ",", Id("arg"), ")", ")", NL,
        WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, Id("i"), "=", Id("i"), "+", Num("1"), NL,
        WS, WS, NL,
        NL,
        NL,
        NL,
        WS, WS, WS, WS, WS, WS, WS, WS, 'return, WS, Id("self"), ".", Id("f"), "(", "*", Id("args"), ",", WS, "**", Id("kwargs"), ")", NL,
        EOS
    )

    preprocess(file_input) shouldParse traceProg2
    (preprocess(file_input) parse traceProg2).size shouldBe 1

    // suite should parse this:
    val dummyin = List[Lexeme](NL,
        WS, 'def, WS, Id("f"), "(", ")", ":", NL,
        WS, WS, 'def, WS, Id("f"), "(", ")", ":", NL,
        WS, WS,   WS, Id("print"), NL,
        WS, WS,   WS, Id("print"), NL,
        WS, WS,   WS, Id("i"), NL)

    //println((suite parse dummyin) mkString "\n\n")

    stmt shouldNotParse List[Lexeme](WS, WS, WS, Id("i"), NL)
    atom shouldNotParse List[Lexeme](WS, WS, WS, Id("i"))

    // This is the skeleton of the python parsers (and it is unambiguous)
    lazy val aStmt: NT[Any] = aSimpleStmt | 'def ~> aBlock
    lazy val aSimpleStmt = a <~ NL
    lazy val aBlock = aSimpleStmt | NL ~> indented(some(many(emptyLine) ~> aStmt))
    lazy val aInput: NT[Any] = NL.* ~> many(aStmt <~ NL.*) <~ EOS

    val dummyin2 = List[Lexeme](
        'def, NL,
        WS, a, NL,
        WS, a, NL,
        WS, 'def, NL,
        WS, WS, a, NL,
        WS, WS, a, NL,
        WS, WS, a, NL,
        NL,
        'def, NL,
        WS, a, NL,
        WS, a, NL,
        WS, 'def, NL,
        WS, WS,WS,WS,WS,WS, a, NL,
        WS, WS,WS,WS,WS,WS, a, NL,
        WS, WS,WS,WS,WS,WS, a, NL,
        EOS)

    aInput shouldParse List[Lexeme](
      'def, NL,
      WS, WS, a, NL,
      WS, WS, a, NL,
      EOS
    )

    aInput shouldNotParse List[Lexeme](
      'def, NL,
      WS, WS, a, NL,
      WS, a, NL,
      EOS
    )

    aInput shouldParse List[Lexeme](
      'def, NL,
      WS, WS, a, NL,
      NL,
      WS, WS, a, NL,
      EOS
    )

    aInput shouldNotParse List[Lexeme](
      'def, NL,
      WS, WS, a, NL,
      NL,
      WS, a, NL,
      EOS
    )

    indentBy(2)(collect) shouldParseWith (
      List[Lexeme](WS, WS, a, NL),
      List[Lexeme](a, NL))

    indentBy(2)(collect) shouldParseWith (
      List[Lexeme](WS, WS, NL, NL, WS, WS, a, NL),
      List[Lexeme](NL, NL, a, NL))

    (aInput parse dummyin2).size shouldBe 1
  }

  // Helpers to allow writing more concise tests.
  private implicit class ParserTests[T, P <% Parser[T]](p: => P) {
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
}
