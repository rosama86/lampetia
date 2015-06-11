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

  trait PgQueryNodeBuilder extends QueryNodeBuilder {
    type N = PgQueryNode
  }

  trait PgQueryNode extends QueryNode {
    override protected def append(operand: Operand): PgQueryNode
    override def select(operands: Operand*)(implicit b: SelectNodeBuilder): PgQueryNode = append(b(operands))
    override def from(operands: Operand*)(implicit b: FromNodeBuilder): PgQueryNode = append(b(operands))
    override def where(operand: Operand)(implicit b: WhereNodeBuilder): PgQueryNode = append(b(operand))
    override def groupBy(operands: Operand*)(implicit b: PrefixNodeBuilder): PgQueryNode = append(b("group by", operands))
    override def having(operands: Operand*)(implicit b: PrefixNodeBuilder): PgQueryNode = append(b("having", operands))
    override def orderBy(operands: Operand*)(implicit b: PrefixNodeBuilder): PgQueryNode = append(b("order by", operands))
    def limit(operands: Operand*)(implicit b: PrefixNodeBuilder): PgQueryNode = append(b("limit", operands))
    def offset(operands: Operand*)(implicit b: PrefixNodeBuilder): PgQueryNode = append(b("offset", operands))
  }

  case class DefaultPgQueryNode(operands: Seq[Operand]) extends PgQueryNode {
    protected def append(operand: Operand): PgQueryNode = copy(operands = operands :+ operand)
    val sqlString: String = s"${operands.map(_.sqlString).mkString(" ")}"
  }

  implicit lazy val QueryNodeBuilder: PgQueryNodeBuilder = new PgQueryNodeBuilder {
    def apply(operands: Seq[Operand]): N = DefaultPgQueryNode(operands)
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
