package fcd
package test
import org.scalatest._

trait LeftrecTests extends CustomMatchers { self: FunSpec with Matchers =>

  import parsers._

  describe("lazyness of alt") {

    describe("p = p | .") {
      lazy val p: NT[Any] = p | any
      p shouldParse "a"
    }

    describe("p = p ~ . | .") {
      lazy val p: NT[_] = p ~ any | any
      p shouldParse "a"
    }

    describe("p = . | p ~ .") {
      lazy val p: NT[_] = any | p ~ any
      p shouldParse "a"
    }

    describe("p = (. | .) >> { (. | p) ^^ id }") {
      lazy val p: NT[Any] = (p | any) flatMap { _ => (any | p) map identity }
      p.shouldParse("aa")
      p.shouldParse("aaaaa")
    }

    describe("p = (. | p) >> { a }") {
      lazy val p: NT[Any] = (any | p) flatMap {  _ => 'a' }
      p.shouldParse("aa")
      p.shouldParse("aaa")
      p.shouldParse("aaaaaa")
    }
  }

  describe("lazyness of seq") {

    describe("p = . ~ p") {
      lazy val p: NT[Any] = any ~ p
      p shouldNotParse "a"
    }

    describe("p = p ~ .") {
      lazy val p: NT[Any] = p ~ any
      p shouldNotParse "a"
    }
  }

  describe("left recursion") {

    describe("A = A ~ a | empty") {
      lazy val A: NT[_] = A ~ 'a' | succeed(42)

      A shouldParse ""
      A shouldParse "a"
      A shouldParse "aa"
    }

    describe("A = empty | A ~ a ") {
      lazy val A: NT[_] = succeed(42) | A ~ 'a'

      A shouldParse ""
      A shouldParse "a"
      A shouldParse "aa"
    }

    // Simple example of indirect leftrecursion from
    //   "Packrat parsers can support left recursion"
    describe("one level indirect leftrecursion") {
      lazy val num: Parser[Any] = many(digit)
      lazy val A: NT[Any] = B ~ '-' ~ num | num
      lazy val B: NT[Any] = succeed(()) ~ A

      // A shouldParse "1"
      // A shouldParse "12"
      // A shouldParse "12-32"
      // A shouldParse "12-32-45"

      B shouldParse "1"
      B shouldParse "12"
      B shouldParse "12-32"
      B shouldParse "12-32-45"
    }

    describe("two levels indirect leftrecursion") {
      lazy val num: Parser[Any] = some(digit)
      lazy val A: NT[Any] = B ~ '-' ~ num | num
      lazy val B: NT[Any] = succeed(()) ~ C ~ '+' ~ num
      lazy val C: NT[Any] = succeed(()) ~ A

      A shouldParse "1"
      A shouldParse "12"
      C shouldParse "2"
      C shouldParse "22"
      B shouldParse "12+32"
      A shouldParse "12+32-42"
      A shouldParse "12+12-32+45-44"
      A shouldNotParse ""
      A shouldNotParse "12+13+14"
      A shouldNotParse "12+13+14-14-56"
    }

    // From "Packrat parsers can support left-recursion"
    describe("super linear parse time") {
      lazy val start: NT[Any] = ones ~ '2' | '1' ~ start | succeed(())
      lazy val ones: NT[Any] = ones ~ '1' | '1'

      start shouldParse ""
      start shouldParse "1"
      start shouldParse "12"
      start shouldParse "11112"
      start shouldParse "111111"
      start shouldParse "1111112"

      // Actually computing the result triggers a stackoverflow
      // start shouldParse ("1" * 200)
    }

    describe("A = A ~ b | c") {
      lazy val A: NT[_] = A ~ 'b' | 'c'

      A shouldParse "c"
      A shouldParse "cb"
      A shouldParse "cbb"
      A shouldParse "cbbbbbbbbbbbbb"
      A shouldNotParse "cbbbbbbbbbbbbbc"
    }

    describe("A = empty ~ A ~ b | empty") {
      lazy val A: NT[Any] = succeed("done") ~ A ~ 'b' | succeed("done")
      A shouldParse ""
      A shouldParse "b"
      A shouldParse "bb"
    }

    // should parse at most as many 'd's as it parses 'b's.
    describe("A = B ~ A ~ b | c\n  B = d | empty") {
      lazy val A: NT[Char] = B ~> A <~ 'b' | 'c'
      lazy val B: NT[_] = charParser('d') | succeed("done")

      A shouldParse "c"
      A shouldParse "cb"
      A shouldParse "dcb"
      A shouldParse "cbb"
      A shouldParse "ddcbb"
      A shouldNotParse "dddcb"
      A shouldParse "dddddcbbbbbbbbbbbbb"
    }

    describe("many(some(a))") {
      lazy val p = many(some('a'))

      p shouldParse ""
      p shouldParse("a")
      p shouldParse("aaa")
      p shouldParse("aaaaaaaaaa")
      p shouldNotParse("b")
      p shouldNotParse("aaab")
    }

    describe("del(ones)") {
      lazy val A: NT[String] = delegate(A) >> { "1" ~> done(_) } | "1"
      A.shouldNotParse("")
      A.shouldParse("1")
      A.shouldParse("11")
      A.shouldParse("111")
    }

    // This grammar is from the paper:
    //   "Packrat Parsers can support left recursion"
    describe("ones") {
      lazy val rr: NT[String] = "1" ~> rr | "1"
      lazy val ll: NT[String] = ll <~ "1" | "1"

      ll shouldParse ("1" * 40)
      rr shouldParse ("1" * 41)
    }

    // Grammar from Tillmann Rendel's GLL library
    describe("very ambiguous") {
      lazy val A: NT[Char] = A ~> A | A ~> A ~> A | 'a'
      A shouldNotParse ""
      A shouldParse "a"
      A shouldParse "aa"
      A shouldParse "aaa"
      A shouldParse ("a" * 100)

      lazy val A2: Parser[Any] = some(some('a'))
      A2 shouldParse ("a" * 1000)
    }

    describe("mixed mutual recursion") {

      lazy val expression: NT[Any] =
        ( literal ~ '+'
        | condExpr
        )

      lazy val condExpr: NT[Any] =
        ( condExpr ~ '?'
        | eqExpr
        )

      lazy val eqExpr: NT[Any] =
        ( eqExpr ~ '*'
        | literal
        )

      lazy val literal: NT[Any] =
        ( many('a')
        | '[' ~ arrayEl
        )

      lazy val arrayEl: NT[Any] =
        ( expression
        | succeed ("undefined")
        )

      expression shouldParse ""
      expression shouldParse "a"
      expression shouldParse "aaaaa"
      expression shouldParse "["
      expression shouldParse "[a"
      expression shouldParse "[aaaaa"
      expression shouldParse "[[[[a"
    }

    describe("terms") {

      trait Term
      case class BinOp(lhs: Term, op: String, rhs: Term) extends Term
      case class Num(n: Int) extends Term

      lazy val term: NT[Term] =
        ( term ~ "+" ~ fact ^^ { case l ~ op ~ r => BinOp(l, op, r) }
        | term ~ "-" ~ fact ^^ { case l ~ op ~ r => BinOp(l, op, r) }
        | fact
        )

      lazy val fact: NT[Term] =
        ( fact ~ "*" ~ num ^^ { case l ~ op ~ r => BinOp(l, op, r) }
        | fact ~ "/" ~ num ^^ { case l ~ op ~ r => BinOp(l, op, r) }
        | num
        )

      lazy val num: Parser[Num] = some(digit) ^^ (ns => Num(ns.mkString.toInt))

      num shouldParse "12345"
      term shouldParse "12+31"
      term shouldParse "12*8+31*45"
    }

    // Grammar and testcases from Tillmann Rendel's GLL library.
    describe("balanced smileys") {
      lazy val az: NT[Any] = acceptIf(c => c >= 'a' && c <= 'z')
      lazy val S: NT[Any] = many(az | ' ' | ':' | ':' ~ P | '(' ~ S ~ ')')
      lazy val P: NT[Any] = charParser('(') | ')'

      S shouldParse ""
      S shouldNotParse ":(("
      S shouldParse "i am sick today (:()"
      S shouldParse "(:)"
      S shouldParse "hacker cup: started :):)"
      S shouldNotParse ")("
      S shouldNotParse "(((a)):()a(()(((:))a((:)():(((()()a)))(:a(::)(a)))(a)((a::():(a)():)a(a(a(:aa(:()(a(((((()))))))))"
      S shouldParse "():)((()():(:())))::aa((((:(((:)))::a:(:))()a)):(a):::((()a((a(aa(():))(():())((::a)a)):)()"
      S shouldParse ":(a):(:)aa)a(:()::():))a:aaa:)(:)((()()))a()(((()(:)))(:(aa:()())())a((a)a:(:()))(a((():)))"
      S shouldParse ":a:)(:))()(()()a)aaa::a()()a:()()a::)((()(a(a))))try implementing sleep sort if you are stuck:(:)a)"
      S shouldNotParse "(a())(::)(a))():(((a(()(:))a(:)))(:(:(:((():)(a))(:))(a)():(:(()aa):)(a((())a)a((a):)()(:("
      S shouldParse "(::a((a)a:()):):a)aa:)a(:::))(a())aa(a():))(:)a)((():)(:a:)a))):a(a)((:()(()())a))()a((()a))"
      S shouldParse "()(((a)((aa)))a)a()(a)(aa:a)()(((:())aa)):()():():a:(a)(a())a:)::a:(aa:):()((a:)())aa)a(a:)"
      S shouldParse ":)()((a)):(():a:a:)(:a)):)(()(:)::::(a(::a())(a):(:((((:(aa(()))a)(((((((((()a()a):)))((:)))))))))"
      S shouldParse "a(a)::(((::)))())((a)(:((:a())):((::(:()(a)))i am trapped in a test case generator :(:(a(:::))"
      S shouldParse "((:):::(()()):)(()()():())aaa)(:(a:)a:((())a(((a(:())aa():a:)((()):)(()(:)(a())a:()a)a():("
      S shouldNotParse "(:a))"
      S shouldParse "::((:))(((:)(aaa)(a())()(a:)(:)(:)()):)a())aa)())(():a):()::):)a()())a()):):(:a)a):()(a)(a)"
      S shouldParse "()a(:)(a:a):(())):a()():((a(:):a()()::)(a:)(()a((a:)(a)a(a:a:)(a)a(a:(()()()::a()a()(()a:())))"
      S shouldParse "()((:a(a()()a))())((:a(:a)(()a((((a((a(()(:aa()()()))):)(():):)(:(a))():(())(():()):):(()a))"
      S shouldParse "(((((((((())))))))))"
      S shouldParse "(((((((((((((((((((())))))))))))))))))))"
      S shouldParse "((((((((((:))))))))))"
      S shouldParse "((((((((((((((((((((((((((((((((((((((((((((((((((:))))))))))))))))))))))))))))))))))))))))))))))))))"
      S shouldNotParse "(((((((((((((((((((((((((((((((((((((((((((((((((((:))))))))))))))))))))))))))))))))))))))))))))))))))"
      S shouldParse "(a((f((g(((g((:))))g))))))::((((((((((((((((((((:)))))))))))))))))))) ((((((((((((((((((((((((((((((((((((((((((((((((((:))))))))))))))))))))))))))))))))))))))))))))))))))"
      S shouldParse "((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:))))))))))"
      S shouldParse "((((((((((((:))))))))))((((((((((:())))))))))))"
      S shouldNotParse "(((((((((()))))))))))"
      S shouldNotParse  "(((((((((((((((((((()))))))))))))))))))))"
      S shouldParse "((((((((((:)))))))))))"
      S shouldParse "(a((f((g(((g((:))))g))))))::((((((((((((((((((((:)))))))))))))))))))) ((((((((((((((((((((((((((((((((((((((((((((((((((:)))))))))))))))))))))))))))))))))))))))))))))))))))"
      S shouldParse "((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))))"
      S shouldParse "((((((((((((:))))))))))((((((((((:)))))))))))))"
      S shouldNotParse "((((((((((:))))))))))))"
      S shouldNotParse "((((((((((((:))))))))))((((((((((:)))))))))))))))"
      S shouldNotParse "((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))) ((((((((((:)))))))))))))))))"
      S shouldNotParse "(a((f((g(((g((:))))g))))))::((((((((((((((((((((:)))))))))))))))))))) ((((((((((((((((((((((((((((((((((((((((((((((((((:))))))))))))))))))))))))))))))))))))))))))))))))))))))"
    }

    // This is grammar Γ₁ from Scott and Johnstone (2010, Sec. 5).
    // taken from Tillmann Rendel's GLL library
    describe("grammar with hidden left recursion") {
      lazy val S: NT[Any] = C ~ 'a' | 'd'
      lazy val B: NT[Any] = succeed(()) | 'a'
      lazy val C: NT[Any] = charParser('b') | B ~ C ~ 'b' | 'b' ~ 'b'

      S shouldNotParse ""
      S shouldNotParse "aba"
      S shouldParse "d"
      S shouldParse "ba"
      S shouldParse "bba"
      S shouldParse "abba"
      S shouldParse "aabbba"
    }
  }
}
