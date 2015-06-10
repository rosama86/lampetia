package lampetia.sql.dialect.postgres

import lampetia.model.sql.DefaultSqlType
import lampetia.sql.ast._
import lampetia.sql.dialect.Dialect

/**
 * @author Hossam Karim
 */

trait PgDsl extends Dsl with Dialect {

  implicit val defaultSqlType: DefaultSqlType = new DefaultSqlType {
    def name: String = "text"
  }

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


  val date = DefaultTypeNode("date")
  val timestamp = DefaultTypeNode("timestamp")
  val json = DefaultTypeNode("json")
  val jsonb = DefaultTypeNode("jsonb")
  def bit(i: Int) = DefaultTypeNode(s"bit($i)")

}
