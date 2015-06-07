package lampetia.sql.dsl.dialect.postgres

import lampetia.sql.ast._

/**
 * @author Hossam Karim
 */

trait PgDsl extends Dsl {

  case class WithNode(alias: IdentifierNode, body: Operand, selection: Operand, recursive: Boolean = false) extends DQLNode {

    def operands: Seq[Operand] = Seq(alias, body, selection)

    def sqlString: String =
      if (recursive)
        s"with recursive ${alias.sqlString} as (${body.sqlString}) ${selection.sqlString}"
      else
        s"with ${alias.sqlString} as (${body.sqlString}) ${selection.sqlString}"
  }

  def `with`(alias: IdentifierNode, body: Operand, selection: Operand) = WithNode(alias, body, selection)
  def withRecursive(alias: IdentifierNode, body: Operand, selection: Operand) = WithNode(alias, body, selection, recursive = true)


  val date = TypeNode("date")
  val timestamp = TypeNode("timestamp")
  val json = TypeNode("json")
  val jsonb = TypeNode("jsonb")
  def bit(i: Int) = TypeNode(s"bit($i)")

}
