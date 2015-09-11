package lampetia.sql.dialect.postgresql

import lampetia.meta.Property
import lampetia.sql.NodeBuilders
import lampetia.sql.ast._

/**
 * @author Hossam Karim
 */

trait PostgresqlDsl extends Dsl with NodeBuilders {



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

  implicit lazy val CastNodeBuilder: CastNodeBuilder = new CastNodeBuilder {
    def apply(operand: Operand, typeNode: TypeNode): CastNode = DefaultPgCastNode(operand, typeNode)
  }

  case class DefaultPgCastNode(operand: Operand, typeNode: TypeNode) extends CastNode {
    val operands: Seq[Operand] = Seq(operand, typeNode)
    val sqlString: String = s"${operand.sqlString}::${typeNode.sqlString}"
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

  trait PgOperandOps[V <: Operand] extends OperandOps[V] {

    // Postgres pattern matching operators
    // http://www.postgresql.org/docs/9.4/static/functions-matching.html
    def similarTo[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" similar to ", value, other)
    def ~[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" ~ ", value, other)
    def ~*[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" ~* ", value, other)
    def !~[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" !~ ", value, other)
    def !~*[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" !~* ", value, other)
  }

  trait PgOperatorOps[V <: Operator] extends OperatorOps[Operator] with PgOperandOps[Operator]

  trait SymbolsDsl extends PgOperandOps[Operand] {
    def symbol: Symbol
    def asIdentifier(implicit b: IdentifierNodeBuilder): IdentifierNode = b(symbol.name)
    def ?(implicit b: NamedParameterNodeBuilder): NamedParameterNode = b(symbol.name)
  }

  implicit class Symbols(val symbol: Symbol) extends SymbolsDsl {
    def value: Operand = asIdentifier
  }

  trait PropertyLifterDsl[A] extends PgOperandOps[Operand] {
    def property: Property[A]
    def asColumnIdentifier(implicit b: ColumnIdentifierNodeBuilder): ColumnIdentifierNode[A] = b[A](property)
  }

  implicit class PropertyLifter[A](val property: Property[A]) extends PropertyLifterDsl[A] {
    def value: Operand = asColumnIdentifier
  }

  implicit class OperandOpsEx[A <: Operand](val value: A) extends PgOperandOps[A]

  implicit class OperatorOpsEx[A <: Operator](val value: A) extends PgOperatorOps[A]

  object Types {
    val date = DefaultTypeNode("date")
    val timestamp = DefaultTypeNode("timestamp")
    val json = DefaultTypeNode("json")
    val jsonb = DefaultTypeNode("jsonb")
    val int = DefaultTypeNode("int")
    def bit(i: Int) = DefaultTypeNode(s"bit($i)")
  }


}
