package lampetia.sql.dialect.mysql

import lampetia.meta.Property
import lampetia.sql.ast._
import lampetia.sql.dialect.Dialect

trait MysqlDsl extends Dsl with Dialect {

  trait MysqlQueryNodeBuilder extends QueryNodeBuilder {
    type N = MysqlQueryNode
  }

  trait MysqlQueryNode extends QueryNode {
    override protected def append(operand: Operand): MysqlQueryNode
    override def select(operands: Operand*)(implicit b: SelectNodeBuilder): MysqlQueryNode = append(b(operands))
    override def from(operands: Operand*)(implicit b: FromNodeBuilder): MysqlQueryNode = append(b(operands))
    override def where(operand: Operand)(implicit b: WhereNodeBuilder): MysqlQueryNode = append(b(operand))
    override def groupBy(operands: Operand*)(implicit b: PrefixNodeBuilder): MysqlQueryNode = append(b("group by", operands))
    override def having(operands: Operand*)(implicit b: PrefixNodeBuilder): MysqlQueryNode = append(b("having", operands))
    override def orderBy(operands: Operand*)(implicit b: PrefixNodeBuilder): MysqlQueryNode = append(b("order by", operands))
    def limit(operands: Operand*)(implicit b: PrefixNodeBuilder): MysqlQueryNode = append(b("limit", operands))
    def offset(operands: Operand*)(implicit b: PrefixNodeBuilder): MysqlQueryNode = append(b("offset", operands))
  }

  case class DefaultMysqlQueryNode(operands: Seq[Operand]) extends MysqlQueryNode {
    protected def append(operand: Operand): MysqlQueryNode = copy(operands = operands :+ operand)
    val sqlString: String = s"${operands.map(_.sqlString).mkString(" ")}"
  }

  implicit lazy val QueryNodeBuilder: MysqlQueryNodeBuilder = new MysqlQueryNodeBuilder {
    def apply(operands: Seq[Operand]): N = DefaultMysqlQueryNode(operands)
  }

  implicit lazy val CastNodeBuilder: CastNodeBuilder = new CastNodeBuilder {
    def apply(operand: Operand, typeNode: TypeNode): CastNode = DefaultMysqlCastNode(operand, typeNode)
  }

  case class DefaultMysqlCastNode(operand: Operand, typeNode: TypeNode) extends CastNode {
    val operands: Seq[Operand] = Seq(operand, typeNode)
    val sqlString: String = s"CAST(${operand.sqlString} AS ${typeNode.sqlString})"
  }

  trait MysqlOperandOps[V <: Operand] extends OperandOps[V]

  trait MysqlOperatorOps[V <: Operator] extends OperatorOps[Operator] with MysqlOperandOps[Operator]

  trait SymbolsDsl extends MysqlOperandOps[Operand] {
    def symbol: Symbol
    def asIdentifier(implicit b: IdentifierNodeBuilder): IdentifierNode = b(symbol.name)
    def ?(implicit b: NamedParameterNodeBuilder): NamedParameterNode = b(symbol.name)
  }

  implicit class Symbols(val symbol: Symbol) extends SymbolsDsl {
    def value: Operand = asIdentifier
  }

  trait PropertyLifterDsl[A] extends MysqlOperandOps[Operand] {
    def property: Property[A]
    def asColumnIdentifier(implicit b: ColumnIdentifierNodeBuilder): ColumnIdentifierNode[A] = b[A](property)
  }

  implicit class PropertyLifter[A](val property: Property[A]) extends PropertyLifterDsl[A] {
    def value: Operand = asColumnIdentifier
  }

  implicit class OperandOpsEx[A <: Operand](val value: A) extends MysqlOperandOps[A]

  implicit class OperatorOpsEx[A <: Operator](val value: A) extends MysqlOperatorOps[A]

}

