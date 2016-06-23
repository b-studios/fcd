package fcd

import scala.language.implicitConversions

/**
 * Additional Case Study: Python Parser
 * ====================================
 * This file contains an additional python parser implementation to support
 * the claims in our paper:
 *
 *   Brachthäuser, Rendel, Ostermann.
 *   Parsing with First-Class Derivatives
 *   Submitted to OOPSLA 2016.
 *
 * The parser is implemented on top of a very simple lexer. The lexer is
 * completely indentation unaware and for instance should lex:
 *
 *   while␣(True):\n
 *   ␣␣a␣*=␣a\n
 *
 * as
 *
 *   KW("while"), WS, Punct("("), KW("True"), Punct(")"), Punct(":"), NL,
 *   WS, WS, Id("a"), Punct("*="), WS, Id("a"), NL
 *
 * Multiline strings should be lexed as instance of Str, with `value` including
 * all of the spaces and newlines that appear in the multiline string.
 *
 * Python programs are then parsed with the parser `preprocess(file_input)`,
 * where `preprocess` in turn is a parser combinator composed of the following
 * three separately defined "stream preprocessing" parser combinators:
 *
 *   1. stripComments    Removes all comment lexemes from the stream
 *   2. explicitJoin     Implements explicit line joining by dropping all
 *                       NL tokens that are preceded by a Punct("\\")
 *   3. implicitJoin     Implements implicit line joining by dropping all
 *                       NL tokens that occur inside pairs of parenthesis.
 *
 * Interestingly, `implicitJoin` itself is defined from components in the
 * following way:
 *
 *   1. The Dyck language of balanced parenthesis is defined (`dyck`)
 *   2. The input to `dyck` is transformed to filter out all non-parenthesis
 *      tokens (`extDyck`)
 *   3. implicitJoin now delegates *all* tokens while it awaits an opening
 *      parenthesis. After seeing such opening parenthesis it filters out
 *      NL when delegating until `extDyck` is successful and thus all pairs of
 *      parens are closed.
 *
 * Indentation senstivity itself is handled in nonterminal `suite` the way it is
 * described in the paper.
 *
 * The python grammar itself is a straightforward translation of:
 * https://docs.python.org/3.5/reference/grammar.html
 */
trait PythonLexemes { self: Parsers with DerivedOps with Syntax =>

  trait Lexeme
  case class Str(value: String) extends Lexeme
  case class Num(value: String) extends Lexeme
  case class KW(name: String) extends Lexeme
  case class Id(name: String) extends Lexeme
  // Punctuation
  case class Punct(sym: String) extends Lexeme
  case object NL extends Lexeme
  case object WS extends Lexeme // whitespace
  case class Comment(content: String) extends Lexeme
  case object EOS extends Lexeme

  type Elem = Lexeme

  implicit def lex(lex: Elem): Parser[Elem] = accept(lex)
  implicit def kw(kw: Symbol): Parser[Elem] = accept(KW(kw.name))
  implicit def punct(p: String): Parser[Elem] = accept(Punct(p))

  lazy val string: Parser[Str] = any flatMap {
    case s: Str => succeed(s)
    case _ => fail
  }
  lazy val number: Parser[Num] = any flatMap {
    case n: Num => succeed(n)
    case _ => fail
  }
  lazy val id: Parser[Id] = any flatMap {
    case id: Id => succeed(id)
    case _ => fail
  }
  lazy val comment: Parser[Comment] = any flatMap {
    case c: Comment => succeed(c)
    case _ => fail
  }

  def isComment: Lexeme => Boolean = _.isInstanceOf[Comment]
  def isNL: Lexeme => Boolean = _ == NL
}

trait PythonParsers extends PythonLexemes with PythonAst { self: Parsers with Syntax with DerivedOps =>

  // general toolbox
  def no(els: Elem*): Parser[Elem] = acceptIf(el => !(els contains el))
  def no(els: Iterable[Elem]): Parser[Elem] = no(els.toSeq : _*)
  def switch[T](p: Elem => Boolean, thn: Elem => Parser[T], els: Elem => Parser[T]): Parser[T] =
    eat { c => if (p(c)) thn(c) else els(c) }

  // Simply preprocesses the input stream and strips out comments
  def stripComments[T]: Parser[T] => Parser[T] = { p =>
    lazy val stripped: Parser[T] =
      done(p) | switch(isComment, _ => stripped, c => stripComments(p << c))
    stripped
  }

  val pairs = Map[Elem, Elem](
    Punct("(") -> Punct(")"),
    Punct("[") -> Punct("]"),
    Punct("{") -> Punct("}"))

  val (opening, closing) = (pairs.keys, pairs.values)

  // non empty Dyck language on these pairs
  val dyck: Parser[Any] = many(oneOf(opening) >> { o => dyck ~ pairs(o) })
  val extDyck = filter((opening ++ closing).toSeq contains _)(dyck)
  // combinator that delegates to p and q until q is successful
  // def until()

  // combinator that only passes the selected lexemes to p
  // def filter[T](pred: Elem => Boolean): Parser[T] => Parser[T] = p => {
  //   val include = some(acceptIf(pred))
  //   val exclude = some(acceptIf(c => !pred(c)))

  //   lazy val filtered: NT[T] =
  //     ( done(p)
  //     | (include &> delegate(p)) >> { pp =>
  //         (exclude ~> filter(pred)(pp)) | done(pp)
  //       }
  //     | exclude ~> filtered
  //     )
  //   filtered
  // }
  def filter[T](pred: Elem => Boolean): Parser[T] => Parser[T] = { p =>
    lazy val filtered: NT[T] =
      ( done(p)
      | eat { el => if (pred(el)) filter(pred)(p << el) else filtered }
      )
    filtered
  }

  // From the python reference manual:
  //
  //   Expressions in parentheses, square brackets or curly braces can be split
  //   over more than one physical line without using backslashes.
  //   [...] Implicitly continued lines can carry comments.
  def implicitJoin[T]: Parser[T] => Parser[T] = { p =>

    // read arbitrary many non-opening tokens
    done(p) | (some(no(opening)) &> delegate(p)).flatMap { pp =>

      // either we are done now, or we actually encounter an opening token
      done(pp) | oneOf(opening).flatMap { o =>

        // filter out all newlines, until closed
        val untilClosed       = o ~ extDyck ~ pairs(o)
        val filterOutNewlines = filter(_ != NL)(delegate(pp))
        ((untilClosed &> filterOutNewlines) << o) >> implicitJoin
      }
    }
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
  def explicitJoin[T]: Parser[T] => Parser[T] = p => {
    lazy val join: NT[T] =
      done(p) | switch(_ == Punct("\\"),
        bs => switch(_ == NL,
          _ => join,
          c => explicitJoin(p << bs << c)),
        c => explicitJoin(p << c))
    join
  }

  val line      = many(no(NL)) ~ NL
  val emptyLine = many(WS) ~ NL
  def indentBy[T](n: Int): Parser[T] => Parser[T] = repeat[T] { p =>
    // pass empty lines as NL to p
    ( emptyLine ^^ { _ => p << NL }
    // first consume `n` spaces, then delegate to p
    // here we intersect with not(emptyLine) to prevent ambiguities
    | manyN(n, WS) ~> (not(emptyLine) &> line &> delegate(p))
    )
  }

  def indented[T](p: Parser[T]): Parser[T] = consumed(some(WS)) >> { case s =>
    // this simulates lookahead for greedy matching
    no(WS) >> { c => indentBy(s.size)(p) <<< s << c }
  }

  def preprocess[T] = stripComments[T] compose explicitJoin[T] compose implicitJoin[T]


  def binOp[T, S](p: Parser[T], op: Parser[S], f: (T, S, T) => T): Parser[T] = {
    lazy val ps: Parser[T] = nonterminal((p ␣ op ␣ ps) ^^ { case l ~ op ~ r => f(l, op, r) } | p)
    ps
  }

  // --- Space Helpers ---

  lazy val whitespace = WS
  lazy val linebreak = NL
  lazy val space = whitespace | linebreak
  lazy val spaces = many(whitespace)

  implicit class SpaceHelpers[T, P <% Parser[T]](p: P) {
    def ␣[U](q: => Parser[U]): Parser[T ~ U] =
      p ~ (spaces ~> q)
    def <␣[U](q: => Parser[U]): Parser[T] =
      p <~ (spaces ~ q)
    def ␣>[U](q: => Parser[U]): Parser[U] =
      p ~> (spaces ~> q)
  }
  def listOf[T](p: Parser[T], sep: Parser[Any]): Parser[List[T]] =
    someSep(p, spaces ~ sep ~ spaces) <~ opt(spaces ~ sep)

  def optList[T](p: Parser[List[T]]) = p | succeed(Nil)

  def spacedOpt[T](p: Parser[T]) = opt(spaces ~> p)
  def spacedMany[T](p: Parser[T]) = many(spaces ~> p)

  // --- Python Grammar ---
  // see: https://docs.python.org/3.5/reference/grammar.html
  lazy val file_input: NT[Program] = emptyLine.* ~> many(stmt <~ emptyLine.*) <~ EOS ^^ Program

  lazy val decorator: Parser[Decorator]  =
    "@" ~> dotted_name ~ ("(" ~> optArgs <~ ")" | succeed(Nil)) <~ NL ^^ Decorator
  lazy val decorators: Parser[List[Decorator]] = some(decorator)
  lazy val decorated: Parser[Decorated]  =
    decorators ~ (classdef | funcdef | async_funcdef) ^^ Decorated


  // --- Functions ---
  lazy val async_funcdef: Parser[FuncDef] = 'async ␣> funcdef
  lazy val funcdef: Parser[FuncDef] =
    'def ␣> (id ␣ parameters ~ spacedOpt("->" ␣> test)) ␣ (":" ␣> suite) ^^ FuncDef

  lazy val parameters: Parser[Any] = "(" ~> spacedOpt(typedargslist) <␣ ")"


  // ['*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef]
  def fpdef(p: Parser[Any]): Parser[Any] =
    ( "*"  ~ spacedOpt(p)
           ~ spacedOpt("," ␣> testdefs(p))
           ~ spacedOpt("," ␣> ("**" ␣ p))
    | "**" ␣ p
    )
  def testdefs(p: Parser[Any]): Parser[Any] = someSep(p ~ spacedOpt("=" ␣> test), ",")

  lazy val typedargslist: Parser[Any] =
    testdefs(tfpdef) ~ spacedOpt("," ␣> fpdef(tfpdef)) | fpdef(tfpdef)

  lazy val varargslist: Parser[Any] =
    testdefs(vfpdef) ~ spacedOpt("," ␣> fpdef(vfpdef)) | fpdef(vfpdef)

  lazy val tfpdef: Parser[Any] = id ~ spacedOpt(":" ␣> test)
  lazy val vfpdef: Parser[Any] = id

  // --- Statements ---
  lazy val stmt: NT[Any]            = simple_stmt | compound_stmt
  lazy val simple_stmt: Parser[Any] = listOf(small_stmt, ";") <␣ NL ^^ Simple
  lazy val small_stmt: Parser[Any]  =
    ( expr_stmt   | del_stmt
    | pass_stmt   | flow_stmt     | import_stmt
    | global_stmt | nonlocal_stmt | assert_stmt
    )

  lazy val expr_stmt: Parser[Any] =
    ( testlist_star_expr
    | testlist_star_expr ␣ augassign ␣ ( yield_expr | testlist )
    | testlist_star_expr ~ some(spaces ~> "=" ␣> ( yield_expr | testlist_star_expr ))
    ) ^^ ExprStmt

  lazy val testlist_star_expr: Parser[Any] = listOf(test | star_expr, ",")

  lazy val augassign: Parser[Any]  = ( "+=" | "-=" | "*=" | "@="  | "/="  | "%="
                                     | "&=" | "|=" | "^=" | "<<=" | ">>=" | "**="
                                     | "//="
                                     )
  lazy val del_stmt: Parser[Stmt]      = 'del ␣> exprlist ^^ Del
  lazy val pass_stmt: Parser[Stmt]     = 'pass ^^^ Pass
  lazy val flow_stmt: Parser[Stmt]     = break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
  lazy val break_stmt: Parser[Stmt]    = 'break ^^^ Break
  lazy val continue_stmt: Parser[Stmt] = 'continue ^^^ Continue
  lazy val return_stmt: Parser[Stmt]   = 'return ~> spacedOpt(testlist) ^^ Return
  lazy val yield_stmt: Parser[Stmt]    = yield_expr ^^ ExprStmt
  lazy val raise_stmt: Parser[Stmt]    = 'raise ~> spacedOpt(test ~ spacedOpt('from ␣ test)) ^^ Raise
  lazy val import_stmt: Parser[Any]    = import_name | import_from
  lazy val import_name: Parser[Any]    = 'import ␣> dotted_as_names ^^ { n => Import(n) }

  // # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
  lazy val import_from: Parser[Any] =
    ('from   ~> (spacedMany("." | "...") ~ dotted_name | some("." | "...")) ␣
     'import ␣> ("*" | "(" ␣> import_as_names <␣ ")" | import_as_names)) ^^ {
        case (from, names) => Import(names, Some(from))
    }

  lazy val import_as_name: Parser[Any]  = id ~ spacedOpt('as ␣ id)
  lazy val dotted_as_name: Parser[Any]  = dotted_name ~ spacedOpt('as ␣ id)
  lazy val import_as_names: Parser[Any] = listOf(test | import_as_name, ",")
  lazy val dotted_as_names: Parser[Any] = someSep(dotted_as_name, ",")
  lazy val dotted_name: Parser[Any]     = someSep(id, ".")

  lazy val global_stmt: Parser[Any]     = 'global ␣> someSep(id, ",") ^^ Global
  lazy val nonlocal_stmt: Parser[Any]   = 'nonlocal ␣> someSep(id, ",") ^^ Nonlocal
  lazy val assert_stmt: Parser[Any]     = 'assert ␣> someSep(test, ",") ^^ Assert


  lazy val compound_stmt: Parser[Any] =
    if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated | async_stmt
  lazy val async_stmt: Parser[Any] = 'async ␣> (funcdef | with_stmt | for_stmt)
  lazy val if_stmt: Parser[Any]    =
    'if ␣> test ␣ (":" ␣> suite ~
      spacedMany('elif ␣> test ␣ (":" ␣> suite)) ~
      spacedOpt(('else ␣ ":") ␣> suite))
  lazy val while_stmt: Parser[Any] =
    'while ␣> test ␣ (":" ␣> suite ~ spacedOpt(('else ␣ ":") ␣> suite))
  lazy val for_stmt: Parser[Any] =
    'for ␣> exprlist ␣ ('in ␣> testlist ␣ (":" ␣> suite ~ spacedOpt(('else ␣> ":") ␣> suite))) ^^ {
      case (exprs ~ (tests ~ (body ~ default))) => For(exprs, tests, body, default)
    }
  lazy val try_stmt: Parser[Any] =
    ('try ␣ ":") ␣> suite ␣ (some(except_clause ␣ (":" ␣> suite)) ~
                               spacedOpt(('else ␣ ":")    ␣> suite) ~
                               spacedOpt(('finally ␣ ":") ␣> suite)
                            | (('finally ␣ ":")     ␣> suite)
                            )
  lazy val with_stmt: Parser[Any] = 'with ␣> someSep(with_item, ",") ␣ (":" ␣> suite)
  lazy val with_item: Parser[Any] = test ~ spacedOpt('as ␣> expr)

  // # NB compile.c makes sure that the default except clause is last
  lazy val except_clause: Parser[Any] = 'except ~> spacedOpt(test ␣ opt('as ␣> id))


  // INDENTATION
  // changed to also allow empty lines
  lazy val suite: Parser[Any] = simple_stmt |  NL ~> indented(some(many(emptyLine) ~> stmt))

  // --- Expressions ---
  lazy val test: NT[Any]           = ( or_test ~ spacedOpt('if ␣> or_test ␣ ('else ␣> test))
                                     | lambdef
                                     )
  lazy val test_nocond: NT[Any]    = or_test | lambdef_nocond
  lazy val lambdef: NT[Any]        = 'lambda ~> spacedOpt(varargslist) ␣ (":" ␣> test)
  lazy val lambdef_nocond: NT[Any] = 'lambda ~> spacedOpt(varargslist) ␣ (":" ␣> test_nocond)
  lazy val or_test: NT[Any]        = someSep(and_test, 'or)
  lazy val and_test: NT[Any]       = someSep(not_test, 'and)
  lazy val not_test: NT[Any]       = 'not ␣> not_test | comparison
  lazy val comparison: NT[Any]     = someSep(expr, comp_op)
  // # <> isn't actually a valid comparison operator in Python. It's here for the
  // # sake of a __future__ import described in PEP 401 (which really works :-)
  lazy val comp_op: Parser[Any] = ( "<" | ">" | "==" | ">=" | "<=" | "<>" | "!="
                                  |'in | 'not ␣ 'in | 'is | 'is ␣ 'not
                                  )

  lazy val expr: NT[Any]       = binOp(xor_expr, "|", BinOp)
  lazy val xor_expr: NT[Any]   = binOp(and_expr, "^", BinOp)
  lazy val and_expr: NT[Any]   = binOp(shift_expr, "&", BinOp)
  lazy val shift_expr: NT[Any] = binOp(arith_expr, "<<" | ">>", BinOp)
  lazy val arith_expr: NT[Any] = binOp(term, "+" | "-", BinOp)
  lazy val term: NT[Any]       = binOp(factor, "*" | "@" | "/" | "%" | "//", BinOp)
  lazy val factor: NT[Any]     = ("+" | "-" | "~") ␣ factor | power
  lazy val power: NT[Any]      = atom_expr | atom_expr ␣ "**" ␣ factor

  lazy val atom_expr: Parser[Any]  = opt('await ~ spaces) ~> atom ~ spacedMany(trailer)
  lazy val atom: Parser[Any] = ( "(" ␣> ( yield_expr | testlist_comp) <␣ ")"
                               | "[" ~> spacedOpt(testlist_comp)  <␣ "]"
                               | "{" ~> spacedOpt(dictorsetmaker) <␣ "}"
                               | id | number | some(string) | "..."
                               | 'None | 'True | 'False
                               )


  lazy val star_expr: Parser[Any] = "*" ␣ expr
  lazy val yield_expr: Parser[Any] = 'yield ~ spacedOpt('from ␣ test | testlist)

  lazy val testlist_comp: Parser[Any] = ( listOf(test | star_expr, ",")
                                        | (test | star_expr) ␣ comp_for
                                        )

  lazy val trailer: Parser[Any] = ( "(" ␣> optArgs  <␣ ")"
                                  | "[" ␣> subscriptlist <␣ "]"
                                  | "." ␣> id
                                  )
  lazy val subscriptlist: Parser[Any] = listOf(subscript, ",")
  lazy val subscript: Parser[Any] = test | spacedOpt(test) ~ ":" ~ spacedOpt(test) ~ spacedOpt(":" ~> spacedOpt(test))
  lazy val exprlist: Parser[List[Any]] = listOf(expr | star_expr, ",")
  lazy val testlist: Parser[Any] = listOf(test, ",")

  lazy val dictorsetmaker: Parser[Any] =
    ( ( listOf(test ␣ (":" ␣> test) | "**" ␣> expr, ",")
      | (test ␣ (":" ␣> test) | "**" ␣> expr) ␣ comp_for
      )
    | ( listOf(test | star_expr, ",")
      | (test | star_expr) ␣ comp_for
      )
    )


  lazy val classdef: Parser[Any] =
    'class ␣> (id ~ spacedOpt("(" ␣> optArgs <␣ ")" )) ␣ (":" ␣> suite)

  lazy val arglist: Parser[List[Any]] = listOf(argument, ",")
  lazy val optArgs: Parser[List[Any]] = arglist | succeed(Nil)

  lazy val argument: Parser[Any] =
    ( test ~ spacedOpt(comp_for)
    | test ␣ "=" ␣ test
    | "**" ␣ test
    | "*" ␣ test
    )

  lazy val comp_iter: NT[Any]   = comp_for | comp_if
  lazy val comp_for: NT[Any]    = 'for ␣> exprlist ␣ ('in ␣> or_test ~ spacedOpt(comp_iter))
  lazy val comp_if: Parser[Any] = 'if ␣> test_nocond ~ spacedOpt(comp_iter)

}

object PythonParsers extends PythonParsers with DerivativeParsers with Syntax with DerivedOps {
  override def accept(t: Elem): Parser[Elem] = acceptIf(_ == t)
}
