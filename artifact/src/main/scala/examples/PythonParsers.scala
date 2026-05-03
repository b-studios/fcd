package fcd

import scala.language.implicitConversions

/** Additional Case Study: Python Parser
  *
  * This file contains an additional python parser implementation to support the
  * claims in our paper.
  *
  * Brachthäuser, Rendel, Ostermann. Parsing with First-Class Derivatives
  * Submitted to OOPSLA 2016.
  *
  * The parser is implemented on top of a very simple lexer. The lexer is
  * completely indentation unaware and for instance should lex:
  *
  * while␣(True):\n ␣␣a␣*=␣a\n
  *
  * as
  *
  * KW("while"), WS, Punct("("), KW("True"), Punct(")"), Punct(":"), NL, WS, WS,
  * Id("a"), Punct("*="), WS, Id("a"), NL
  *
  * Multiline strings should be lexed as instance of Str, with `value` including
  * all of the spaces and newlines that appear in the multiline string.
  *
  * Python programs are then parsed with the parser `preprocess(file_input)`,
  * where `preprocess` in turn is a parser combinator composed of the following
  * three separately defined "stream preprocessing" parser combinators:
  *
  *   1. stripComments Removes all comment lexemes from the stream 2.
  *      explicitJoin Implements explicit line joining by dropping all NL tokens
  *      that are preceded by a Punct("\\") 3. implicitJoin Implements implicit
  *      line joining by dropping all NL tokens that occur inside pairs of
  *      parenthesis.
  *
  * Interestingly, `implicitJoin` itself is defined from components in the
  * following way:
  *
  *   1. The Dyck language of balanced parenthesis is defined (`dyck`) 2. The
  *      input to `dyck` is transformed to filter out all non-parenthesis tokens
  *      (`extDyck`) 3. implicitJoin now delegates *all* tokens while it awaits
  *      an opening parenthesis. After seeing such opening parenthesis it
  *      filters out NL when delegating until `extDyck` is successful and thus
  *      all pairs of parens are closed.
  *
  * Indentation senstivity itself is handled in nonterminal `suite` the way it
  * is described in the paper.
  *
  * The python grammar itself is a straightforward translation of:
  * https://docs.python.org/3.5/reference/grammar.html
  */
trait PythonLexemes { self: Parsers & DerivedOps & Syntax =>
  enum Lexeme {
    case Str(value: String)
    case Num(value: String)
    case KW(name: String)
    case Id(name: String)
    case Punct(sym: String)
    case Comment(content: String)
    case NL
    case WS
    case EOS
  }

  import Lexeme._

  type Elem = Lexeme

  given lex: Conversion[Elem, Parser[Elem]] = accept(_)
  given kw: Conversion[String, Parser[Elem]] = kw => accept(KW(kw))
  given punct: Conversion[Char, Parser[Elem]] = p => accept(Punct(p.toString))

  lazy val string = any >> {
    case s: Str => succeed(s)
    case _      => fail
  }
  lazy val number = any >> {
    case n: Num => succeed(n)
    case _      => fail
  }
  lazy val id = any >> {
    case id: Id => succeed(id)
    case _      => fail
  }
  lazy val comment = any >> {
    case c: Comment => succeed(c)
    case _          => fail
  }

  def isComment(x: Lexeme): Boolean = x.isInstanceOf[Comment]
  def isNL(x: Lexeme): Boolean = x == NL
}

trait PythonParsers extends PythonLexemes {
  self: Parsers & Syntax & DerivedOps =>

  import PythonAst.*
  import Stmt._

  // general toolbox
  def no(els: Elem*): Parser[Elem] = acceptIf(el => !(els contains el))
  def no(els: Iterable[Elem]): Parser[Elem] = no(els.toSeq*)
  def switch[T](
      p: Elem => Boolean,
      thn: Elem => Parser[T],
      els: Elem => Parser[T]
  ) = eat { c => if (p(c)) thn(c) else els(c) }

  // Simply preprocesses the input stream and strips out comments
  def stripComments[T](p: Parser[T]): Parser[T] = {
    lazy val stripped: Parser[T] =
      done(p) | switch(isComment(_), _ => stripped, c => stripComments(p << c))
    stripped
  }

  import Lexeme._

  val pairs = Map[Elem, Elem](
    Punct("(") -> Punct(")"),
    Punct("[") -> Punct("]"),
    Punct("{") -> Punct("}")
  )

  val (opening, closing) = (pairs.keys, pairs.values)

  def enclosed[T](p: => Parser[T]) = oneOf(opening) >> { o => p <~ pairs(o) }

  // non empty Dyck language on these pairs
  lazy val dyck: Parser[Any] = enclosed(many(dyck))

  // the repetition of enclosed is unfortunate
  lazy val extDyck = enclosed(always) &>
    filter((opening ++ closing).toSeq contains _)(dyck)

  // From the python reference manual:
  //
  //   Expressions in parentheses, square brackets or curly braces can be split
  //   over more than one physical line without using backslashes.
  //   [...] Implicitly continued lines can carry comments.
  def implicitJoin[T]: Parser[T] => Parser[T] = repeat[T] { p =>
    (extDyck &> filter(_ != NL)(delegate(p))
      | noneOf(opening ++ closing) &> delegate(p))
  }

  // Strips out newlines if they are preceeded by a backslash punctuation
  // This is no way special and could already been performed by the lexer.
  //
  // From the python reference manual:
  //
  //   Two or more physical lines may be joined into logical lines using
  //   backslash characters (\), as follows: when a physical line ends in a
  //   backslash that is not part of a string literal or comment, it is joined
  //   with the following forming a single logical line, deleting the backslash
  //   and the following end-of-line character.
  def explicitJoin[T](p: Parser[T]): Parser[T] = {
    lazy val join: NT[T] =
      done(p) | switch(
        _ == Punct("\\"),
        bs => switch(_ == NL, _ => join, c => explicitJoin(p << bs << c)),
        c => explicitJoin(p << c)
      )
    join
  }

  val line = many(no(NL)) ~ NL
  val emptyLine = many(WS) ~ NL
  def indentBy[T](indentation: Parser[Any]) = repeat[T] { p =>
    // here we use (locally) biased choice to prevent ambiguities
    biasedAlt(
      // pass empty lines as NL to p
      emptyLine ^^ { _ => p << NL },
      // first consume `n` spaces, then delegate to p
      indentation ~> (line &> delegate(p))
    )
  }

  def indented[T](p: Parser[T]) =
    consumed(some(WS)) >> { i =>
      not(prefix(WS)) &> indentBy(acceptSeq(i))(p) <<< i
    }

  def preprocess[T] =
    stripComments[T] compose explicitJoin[T] compose implicitJoin[T]

  def binOp[T, S](p: Parser[T], op: Parser[S], f: (T, S, T) => T) = {
    lazy val ps: Parser[T] = nonterminal((p ␣ op ␣ ps) ^^ { case ((l, op), r) =>
      f(l, op, r)
    } | p)
    ps
  }

  // --- Space Helpers ---

  val whitespace = WS
  val linebreak = NL
  val space = alt(whitespace, linebreak)
  lazy val spaces = many(whitespace)

  extension [T](p: Parser[T]) {
    def ␣[U](q: => Parser[U]) = p ~ (spaces ~> q)
    def <␣[U](q: => Parser[U]) = p <~ (spaces ~ q)
    def ␣>[U](q: => Parser[U]) = p ~> (spaces ~> q)
  }

  def listOf[T](p: Parser[T], sep: Parser[Any]) =
    someSep(p, spaces ~ sep ~ spaces) <~ opt(spaces ~ sep)

  def optList[T](p: Parser[List[T]]) = p | succeed(Nil)

  def spacedOpt[T](p: Parser[T]) = opt(spaces ~> p)
  def spacedMany[T](p: Parser[T]) = many(spaces ~> p)

  // --- Python Grammar ---
  // see: https://docs.python.org/3.5/reference/grammar.html
  lazy val file_input: NT[Program] =
    emptyLine.* ~> many(stmt <~ emptyLine.*) <~ EOS ^^ Program.apply

  lazy val decorator: Parser[Decorator] =
    '@' ~> dotted_name ~ ('(' ~> optArgs <~ ')' | succeed(
      Nil
    )) <~ NL ^^ Decorator.apply
  lazy val decorators: Parser[List[Decorator]] = some(decorator)
  lazy val decorated: Parser[Decorated] =
    decorators ~ (classdef | funcdef | async_funcdef) ^^ Decorated.apply

  // --- Functions ---
  lazy val async_funcdef: Parser[FuncDef] = "async" ␣> funcdef
  lazy val funcdef: Parser[FuncDef] =
    "def" ␣> (id ␣ parameters ~ spacedOpt(
      Punct("->") ␣> test
    )) ␣ (':' ␣> suite) ^^ FuncDef.apply

  lazy val parameters = '(' ~> spacedOpt(typedargslist) <␣ ')'

  // ['*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef]
  def fpdef(p: Parser[Any]): Parser[Any] =
    ('*' ~ spacedOpt(p)
      ~ spacedOpt(',' ␣> testdefs(p))
      ~ spacedOpt(',' ␣> (Punct("**") ␣ p))
      | Punct("**") ␣ p)
  def testdefs(p: Parser[Any]) = someSep(p ~ spacedOpt('=' ␣> test), ',')

  lazy val typedargslist =
    testdefs(tfpdef) ~ spacedOpt(',' ␣> fpdef(tfpdef)) | fpdef(tfpdef)

  lazy val varargslist =
    testdefs(vfpdef) ~ spacedOpt(',' ␣> fpdef(vfpdef)) | fpdef(vfpdef)

  lazy val tfpdef = id ~ spacedOpt(':' ␣> test)
  lazy val vfpdef = id

  // --- Statements ---
  lazy val stmt: NT[Any] = simple_stmt | compound_stmt
  lazy val simple_stmt =
    listOf(small_stmt, ';') <␣ NL ^^ Simple.apply
  lazy val small_stmt =
    (expr_stmt | del_stmt
      | pass_stmt | flow_stmt | import_stmt
      | global_stmt | nonlocal_stmt | assert_stmt)

  lazy val expr_stmt =
    (testlist_star_expr
      | testlist_star_expr ␣ augassign ␣ (yield_expr | testlist)
      | testlist_star_expr ~ some(
        spaces ~> '=' ␣> (yield_expr | testlist_star_expr)
      )) ^^ ExprStmt.apply

  lazy val testlist_star_expr = listOf(test | star_expr, ',')

  lazy val augassign = (Punct("+=") | Punct("-=") | Punct("*=") | Punct(
    "@=") | Punct("/=") | Punct("%=")
    | Punct("&=") | Punct("|=") | Punct("^=") | Punct("<<=") | Punct(
      ">>=") | Punct("**=") | Punct("//="))
  lazy val del_stmt = "del" ␣> exprlist ^^ Del.apply
  lazy val pass_stmt = "pass" ^^^ Pass
  lazy val flow_stmt =
    break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
  lazy val break_stmt = "break" ^^^ Break
  lazy val continue_stmt = "continue" ^^^ Continue
  lazy val return_stmt = "return" ~> spacedOpt(testlist) ^^ Return.apply
  lazy val yield_stmt = yield_expr ^^ ExprStmt.apply
  lazy val raise_stmt =
    "raise" ~> spacedOpt(test ~ spacedOpt("from" ␣ test)) ^^ Raise.apply
  lazy val import_stmt = import_name | import_from
  lazy val import_name = "import" ␣> dotted_as_names ^^ { Import(_) }

  // # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
  lazy val import_from =
    ("from" ~> (spacedMany('.' | Punct("...")) ~ dotted_name | some(
      '.' | Punct("...")
    )) ␣
      "import"
      ␣> ('*' | '(' ␣> import_as_names <␣ ')' | import_as_names)) ^^ {
      case (from, names) => Import(names, Some(from))
    }

  lazy val import_as_name = id ~ spacedOpt("as" ␣ id)
  lazy val dotted_as_name = dotted_name ~ spacedOpt("as" ␣ id)
  lazy val import_as_names = listOf(test | import_as_name, ',')
  lazy val dotted_as_names = someSep(dotted_as_name, ',')
  lazy val dotted_name = someSep(id, '.')

  lazy val global_stmt = "global" ␣> someSep(id, ',') ^^ Global.apply
  lazy val nonlocal_stmt = "nonlocal" ␣> someSep(id, ',') ^^ Nonlocal.apply
  lazy val assert_stmt = "assert" ␣> someSep(test, ',') ^^ Assert.apply

  lazy val compound_stmt =
    if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated | async_stmt
  lazy val async_stmt =
    "async" ␣> (funcdef | with_stmt | for_stmt)
  lazy val if_stmt =
    "if" ␣> test ␣ (':' ␣> suite ~
      spacedMany("elif" ␣> test ␣ (':' ␣> suite)) ~
      spacedOpt(("else" ␣ ':') ␣> suite))
  lazy val while_stmt =
    "while" ␣> test ␣ (':' ␣> suite ~ spacedOpt(("else" ␣ ':') ␣> suite))
  lazy val for_stmt =
    "for" ␣> exprlist ␣ ("in" ␣> testlist ␣ (':' ␣> suite ~ spacedOpt(
      ("else" ␣> ':') ␣> suite
    ))) ^^ { case (exprs, (tests, (body, default))) =>
      For(exprs, tests, body, default)
    }
  lazy val try_stmt =
    ("try" ␣ ':') ␣> suite ␣ (some(except_clause ␣ (':' ␣> suite)) ~
      spacedOpt(("else" ␣ ':') ␣> suite) ~
      spacedOpt(("finally" ␣ ':') ␣> suite)
      | (("finally" ␣ ':') ␣> suite))
  lazy val with_stmt = "with" ␣> someSep(with_item, ',') ␣ (':' ␣> suite)
  lazy val with_item = test ~ spacedOpt("as" ␣> expr)

  // # NB compile.c makes sure that the default except clause is last
  lazy val except_clause = "except" ~> spacedOpt(test ␣ opt("as" ␣> id))

  // INDENTATION
  // changed to also allow empty lines
  lazy val suite = simple_stmt | NL ~> indented(some(many(emptyLine) ~> stmt))

  // --- Expressions ---
  lazy val test: NT[Any] =
    (or_test ~ spacedOpt("if" ␣> or_test ␣ ("else" ␣> test))
      | lambdef)
  lazy val test_nocond: NT[Any] = or_test | lambdef_nocond
  lazy val lambdef: NT[Any] = "lambda" ~> spacedOpt(varargslist) ␣ (':' ␣> test)
  lazy val lambdef_nocond: NT[Any] =
    "lambda" ~> spacedOpt(varargslist) ␣ (':' ␣> test_nocond)
  lazy val or_test: NT[Any] = someSep(and_test, "or")
  lazy val and_test: NT[Any] = someSep(not_test, "and")
  lazy val not_test: NT[Any] = "not" ␣> not_test | comparison
  lazy val comparison: NT[Any] = someSep(expr, comp_op)
  // # <> isn't actually a valid comparison operator in Python. It's here for the
  // # sake of a __future__ import described in PEP 401 (which really works :-)
  lazy val comp_op =
    Punct("<") | ">" | Punct("==") | Punct(">=") | Punct("<=") | Punct(
      "<>") | Punct("!=") | "in" | "not" ␣ "in" | "is" | "is" ␣ "not"

  lazy val expr: NT[Any] = binOp(xor_expr, '|', BinOp.apply)
  lazy val xor_expr: NT[Any] = binOp(and_expr, '^', BinOp.apply)
  lazy val and_expr: NT[Any] = binOp(shift_expr, '&', BinOp.apply)
  lazy val shift_expr: NT[Any] =
    binOp(arith_expr, Punct("<<") | Punct(">>"), BinOp.apply)
  lazy val arith_expr: NT[Any] = binOp(term, Punct("+") | "-", BinOp.apply)
  lazy val term: NT[Any] =
    binOp(factor, Punct("*") | '@' | '/' | '%' | Punct("//"), BinOp.apply)
  lazy val factor: NT[Any] = (Punct("+") | '-' | '~') ␣ factor | power
  lazy val power: NT[Any] = atom_expr | atom_expr ␣ Punct("**") ␣ factor
  lazy val atom_expr =
    opt("await" ~ spaces) ~> atom ~ spacedMany(trailer)
  lazy val atom = ('(' ␣> (yield_expr | testlist_comp) <␣ ')'
    | '[' ~> spacedOpt(testlist_comp) <␣ ']'
    | '{' ~> spacedOpt(dictorsetmaker) <␣ '}'
    | id | number | some(string) | Punct("...")
    | "None" | "True" | "False")

  lazy val star_expr = '*' ␣ expr
  lazy val yield_expr = "yield" ~ spacedOpt("from" ␣ test | testlist)

  lazy val testlist_comp = (listOf(test | star_expr, ',')
    | (test | star_expr) ␣ comp_for)

  lazy val trailer = ('(' ␣> optArgs <␣ ')'
    | '[' ␣> subscriptlist <␣ ']'
    | '.' ␣> id)
  lazy val subscriptlist = listOf(subscript, ',')
  lazy val subscript =
    test | spacedOpt(test) ~ ':' ~ spacedOpt(test) ~ spacedOpt(
      ':' ~> spacedOpt(test)
    )
  lazy val exprlist = listOf(expr | star_expr, ',')
  lazy val testlist = listOf(test, ',')

  lazy val dictorsetmaker =
    ((listOf(test ␣ (':' ␣> test) | Punct("**") ␣> expr, ',')
      | (test ␣ (':' ␣> test) | Punct("**") ␣> expr) ␣ comp_for)
      | (listOf(test | star_expr, ',')
        | (test | star_expr) ␣ comp_for))

  lazy val classdef =
    "class" ␣> (id ~ spacedOpt('(' ␣> optArgs <␣ ')')) ␣ (':' ␣> suite)

  lazy val arglist = listOf(argument, ',')
  lazy val optArgs = arglist | succeed(Nil)

  lazy val argument = (test ~ spacedOpt(comp_for)
    | test ␣ '=' ␣ test
    | Punct("**") ␣ test
    | '*' ␣ test)

  lazy val comp_iter: NT[Any] = comp_for | comp_if
  lazy val comp_for =
    "for" ␣> exprlist ␣ ("in" ␣> or_test ~ spacedOpt(comp_iter))
  lazy val comp_if = "if" ␣> test_nocond ~ spacedOpt(comp_iter)

}

object PythonParsers
    extends PythonParsers,
      DerivedOps,
      DerivativeParsers,
      Syntax {
  override def accept(t: Elem) = acceptIf(_ == t)
}
