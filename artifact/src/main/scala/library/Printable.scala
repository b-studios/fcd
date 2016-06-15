package fcd

import language.postfixOps
import scala.sys.process._
import java.io.ByteArrayInputStream

// Print-Helpers and other debugging tools
trait Printable {

  def results: List[Any]
  def failed: Boolean
  def name: String

  lazy val table =
    <table border="0" cellborder="1" cellspacing="0" cellpadding="4">
      <tr><td><font point-size="10"><b>{name}</b></font></td><td>{id}</td></tr>
      <tr><td>failed?</td><td>{failed}</td></tr>
      <tr><td>results</td><td>{results.toSet.mkString(", ")}</td></tr>
    </table>


  private lazy val printGraph: String =
    s"""strict digraph G {
       |  ${printNode}
       |}
       |""".stripMargin('|')

  def printToFile(path: String): Unit = {
    val is = new ByteArrayInputStream(printGraph.getBytes("UTF-8"))
    (s"dot -Tpng -o $path" #< is) !
  }

  def id = f"n${this.hashCode}%x"

  def printNode: String
}

abstract class NullaryPrintable(val name: String) extends Printable {
  def printNode = s"""$id [label="$name", shape=circle]"""
}

abstract class UnaryPrintable(val name: String, _p: => Printable) extends Printable {
  private lazy val p = _p
  def printNode =
    s"""  ${id} [shape=none, fontsize=8, fontname=mono, label=<$table>];
       |  ${id}:s -> ${p.id}
       |${p.printNode}""".stripMargin('|')
}

// we might run into problems with defining nodes multiple times, let's see.
abstract class BinaryPrintable(val name: String, p: Printable, q: Printable) extends Printable {
  def printNode =
    s"""  ${id} [shape=none, fontsize=8, fontname=mono, label=<$table>];
       |  ${id}:sw -> ${p.id}
       |  ${id}:se -> ${q.id}
       |${p.printNode}
       |${q.printNode}""".stripMargin('|')
}
