package fcd

trait PythonAst {
  trait Tree

  case class Program(stmts: Seq[Any]) extends Tree

  case class Decorator(name: Any, args: Seq[Any]) extends Tree
  case class Decorated(decorators: Seq[Decorator], el: Any) extends Tree

  trait Def extends Tree
  case class FuncDef(name: Any, params: Any, retAnnot: Option[Any], body: Any) extends Def

  trait Stmt extends Tree
  case class Simple(small: Seq[Any]) extends Stmt

  case class Del(exprs: Seq[Any]) extends Stmt
  case object Pass extends Stmt
  case object Break extends Stmt
  case object Continue extends Stmt
  case class Return(expr: Option[Any]) extends Stmt
  case class Raise(expr: Option[Any]) extends Stmt
  case class ExprStmt(expr: Any) extends Stmt
  case class Import(names: Any, from: Option[Any] = None) extends Stmt

  case class Global(ids: Seq[Any]) extends Stmt
  case class Nonlocal(ids: Seq[Any]) extends Stmt
  case class Assert(tests: Seq[Any]) extends Stmt

  case class For(exprs: Seq[Any], in: Any, body: Any, default: Any) extends Stmt

  trait Expr extends Tree
  case class BinOp(l: Any, op: Any, r: Any) extends Expr
}
