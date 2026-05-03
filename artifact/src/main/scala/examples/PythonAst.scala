package fcd

object PythonAst {
  trait Tree
  case class Program(stmts: Seq[Any]) extends Tree

  case class Decorator(name: Any, args: Seq[Any]) extends Tree
  case class Decorated(decorators: Seq[Decorator], el: Any) extends Tree

  trait Def extends Tree
  case class FuncDef(name: Any, params: Any, retAnnot: Option[Any], body: Any)
      extends Def

  enum Stmt extends Tree {
    case Simple(small: Seq[Any])
    case Del(exprs: Seq[Any])
    case Pass
    case Break
    case Continue
    case Return(expr: Option[Any])
    case Raise(expr: Option[Any])
    case ExprStmt(expr: Any)
    case Import(names: Any, from: Option[Any] = None)
    case Global(ids: Seq[Any])
    case Nonlocal(ids: Seq[Any])
    case Assert(tests: Seq[Any])
    case For(exprs: Seq[Any], in: Any, body: Any, default: Any)
  }

  trait Expr extends Tree
  case class BinOp(l: Any, op: Any, r: Any) extends Expr
}
