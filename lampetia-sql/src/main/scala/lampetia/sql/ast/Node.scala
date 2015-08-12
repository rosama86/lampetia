package lampetia.sql.ast

import lampetia.meta.{Property, Model}
import lampetia.meta.feature.sql._

import scala.language.implicitConversions

/**
 * @author Hossam Karim
 */

trait Node {
  def sqlString: String
}

trait Operand extends Node

trait TypedOperand[+T] extends Operand {
  def value: T
}

/*
case class TypedOperandAdapter[T](typedOperand: TypedOperand[T], adaptee: Operand) extends TypedOperand[T] {
  val value = typedOperand.value
  val sqlString = adaptee.sqlString
}
*/

trait Operator extends Operand {
  def operands: Seq[Operand]
}

trait UnaryOperator[O <: Operand] extends Operator {
  def operand: O
  val operands: Seq[Operand] = Seq(operand)
}

trait BinaryOperator[A <: Operand, B <: Operand] extends Operator {
  def first: A
  def second: B
  val operands: Seq[Operand] = Seq(first, second)
}

trait DQLNode extends Operator

trait DMLNode extends Operator

trait DDLNode extends Operator

trait LiteralNode extends Operand

trait StringLiteralNodeBuilder {
  def apply(value: String): StringLiteralNode
}
trait StringLiteralNode extends LiteralNode {
  def value: String
}

case class DefaultStringLiteralNode(value: String) extends StringLiteralNode {
  val sqlString: String = s"'$value'"
}

trait IntegerLiteralNodeBuilder {
  def apply(value: Int): IntegerLiteralNode
}
trait IntegerLiteralNode extends LiteralNode {
  def value: Int
}

case class DefaultIntegerLiteralNode(value: Int) extends IntegerLiteralNode {
  val sqlString: String = s"$value"
}

trait IdentifierNodeBuilder {
  def apply(name: String): IdentifierNode
}

trait IdentifierNode extends Operand {
  def name: String
}

case class DefaultIdentifierNode(name: String) extends IdentifierNode {
  val sqlString: String = name
}


trait TableIdentifierNodeBuilder {
  def apply[A](model: Model[A]): TableIdentifierNode[A]
}

trait TableIdentifierNode[A] extends Operand {
  def model: Model[A]
  def as(alias: IdentifierNode)(implicit b: InfixNodeBuilder): InfixNode = b(" as ", this, alias)
  def join[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" join ", this, other)
  def innerJoin[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" inner join ", this, other)
  def leftJoin[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" left join ", this, other)
  def rightJoin[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" right join ", this, other)
  def leftOuterJoin[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" left outer join ", this, other)
}

case class DefaultTableIdentifierNode[A](model: Model[A]) extends TableIdentifierNode[A] {
  val sqlString: String = model.sqlName
}

trait ColumnIdentifierNodeBuilder {
  def apply[A](property: Property[A]): ColumnIdentifierNode[A]
}
trait ColumnIdentifierNode[A] extends Operand {
  def property: Property[A]
}

case class DefaultColumnIdentifierNode[E, A](property: Property[A]) extends ColumnIdentifierNode[A] {
  val sqlString: String = property.sqlName
}

trait ParameterNodeBuilder {
  def apply: ParameterNode
}
trait ParameterNode extends Operand

case object DefaultParameterNode extends ParameterNode {
  val sqlString: String = "?"
}

trait NamedParameterNodeBuilder {
  def apply(parameterName: String): NamedParameterNode
}
trait NamedParameterNode extends Operand {
  def parameterName: String
}

case class DefaultNamedParameterNode(parameterName: String) extends NamedParameterNode {
  val sqlString: String = s"#{$parameterName}"
}

trait InfixNodeBuilder {
  def apply(symbol: String, first: Operand, second: Operand, groupSecond: Boolean = false): InfixNode
}

trait InfixNode extends BinaryOperator[Operand, Operand]{
  def symbol: String
  def first: Operand
  def second: Operand
  def groupSecond: Boolean
}

case class DefaultInfixNode(symbol: String, first: Operand, second: Operand, groupSecond: Boolean = false) extends InfixNode {
  val sqlString: String =
    if (groupSecond)
      s"${first.sqlString}$symbol(${second.sqlString})"
    else
      s"${first.sqlString}$symbol${second.sqlString}"
}

trait CastNode extends Operator {
  def operand: Operand
  def typeNode: TypeNode
}

trait CastNodeBuilder {
  def apply(operand: Operand, typeNode: TypeNode): CastNode
}

trait PrefixNodeBuilder {
  def apply(symbol: String, operands: Seq[Operand]): PrefixNode
}

trait PrefixNode extends Operator {
  def symbol: String
  def operands: Seq[Operand]
}

case class DefaultPrefixNode(symbol: String, operands: Seq[Operand]) extends PrefixNode {
  val sqlString: String = s"$symbol ${operands.map(_.sqlString).mkString(",")}"
}

trait PostfixNodeBuilder {
  def apply(symbol: String, operands: Seq[Operand]): PostfixNode
}

trait PostfixNode extends Operator {
  def symbol: String
  def operands: Seq[Operand]
}

case class DefaultPostfixNode(symbol: String, operands: Seq[Operand]) extends PostfixNode {
  val sqlString: String = s"${operands.map(_.sqlString).mkString(",")} $symbol"
}

trait SurroundNodeBuilder {
  def apply(operand: Operand): SurroundNode
}

trait SurroundNode {
  def operand: Operand
}

case class DefaultSurroundNode(operand: Operand) extends SurroundNode {
  val operands: Seq[Operand] = Seq(operand)
  val sqlString: String = s"(${operand.sqlString})"
}

trait TypeNodeBuilder {
  def apply(typeName: String): TypeNode
}

trait TypeNode extends Operand {
  def typeName: String
}

case class DefaultTypeNode(typeName: String) extends TypeNode {
  val sqlString: String = typeName
}

trait BetweenNodeBuilder {
  def apply(lhs: Operand, first: Operand, second: Operand): BetweenNode
}

trait BetweenNode extends Operator {
  def lhs: Operand
  def first: Operand
  def second: Operand
}

case class DefaultBetweenNode(lhs: Operand, first: Operand, second: Operand) extends BetweenNode {
  val operands: Seq[Operand] = Seq(lhs, first, second)
  val sqlString: String = s"${lhs.sqlString} between ${first.sqlString} and ${second.sqlString}"
}



trait AndNodeBuilder {
  def apply(operands: Seq[Operand]): AndNode
}
trait AndNode extends Operator {
  def and(other: Operand)(implicit ab: AndNodeBuilder): AndNode =
    ab(operands :+ other)
}

case class DefaultAndNode(operands: Seq[Operand]) extends AndNode {
  val sqlString: String = s"(${operands.map(_.sqlString).mkString(" and ")})"
}

trait OrNodeBuilder {
  def apply(operands: Seq[Operand]): OrNode

}
trait OrNode extends Operator {
  def or(other: Operand)(implicit ob: OrNodeBuilder): OrNode =
    ob(operands :+ other)
}

case class DefaultOrNode(operands: Seq[Operand]) extends OrNode {
  val sqlString: String = s"(${operands.map(_.sqlString).mkString(" or ")})"
}

trait NotNodeBuilder {
  def apply(operand: Operand): NotNode
}

trait NotNode extends UnaryOperator[Operand] {
  def operand: Operand
}

case class DefaultNotNode(operand: Operand) extends NotNode {
  val sqlString: String = s"(not ${operand.sqlString})"
}

trait FunctionNodeBuilder {
  def apply(name: String, operands: Seq[Operand]): FunctionNode
}

trait FunctionNode extends Operator {
  def name: String
  def operands: Seq[Operand]
}

case class DefaultFunctionNode(name: String, operands: Seq[Operand]) extends FunctionNode {
  val sqlString: String = s"$name(${operands.map(_.sqlString).mkString(",")})"
}

trait QueryNode extends DQLNode {
  protected def append(operand: Operand): QueryNode
  def select(operands: Operand*)(implicit b: SelectNodeBuilder): QueryNode = append(b(operands))
  def from(operands: Operand*)(implicit b: FromNodeBuilder): QueryNode = append(b(operands))
  def where(operand: Operand)(implicit b: WhereNodeBuilder): QueryNode = append(b(operand))
  def groupBy(operands: Operand*)(implicit b: PrefixNodeBuilder): QueryNode = append(b("group by", operands))
  def having(operands: Operand*)(implicit b: PrefixNodeBuilder): QueryNode = append(b("having", operands))
  def orderBy(operands: Operand*)(implicit b: PrefixNodeBuilder): QueryNode = append(b("order by", operands))
}

trait QueryNodeBuilder {
  type N <: QueryNode
  def apply(operands: Seq[Operand]): N
}

case class DefaultQueryNode(operands: Seq[Operand]) extends QueryNode {
  protected def append(operand: Operand): QueryNode = copy(operands = operands :+ operand)
  val sqlString: String = s"${operands.map(_.sqlString).mkString(" ")}"
}

trait SelectNodeBuilder {
  def apply(operands: Seq[Operand]): SelectNode
}

trait SelectNode extends DQLNode
  
case class DefaultSelectNode(operands: Seq[Operand]) extends SelectNode {
  val sqlString: String = s"select ${operands.map(_.sqlString).mkString(",")}"
}

trait FromNodeBuilder {
  def apply(operands: Seq[Operand]): FromNode
}
trait FromNode extends DQLNode {
  def operands: Seq[Operand]
}

case class DefaultFromNode(operands: Seq[Operand]) extends FromNode {
  val sqlString: String = s"from ${operands.map(_.sqlString).mkString(",")}"
}

trait WhereNode extends DQLNode with UnaryOperator[Operand]

trait WhereNodeBuilder {
  def apply(operand: Operand): WhereNode
}

case class DefaultWhereNode(operand: Operand) extends WhereNode {
  val sqlString: String = s"where ${operands.map(_.sqlString).mkString(" ")}"
}

sealed trait JoinType {
  def joinString: String
}

case object NaturalJoin extends JoinType {
  val joinString: String = "join"
}
case object InnerJoin extends JoinType {
  val joinString: String = "inner join"
}
case object LeftJoin extends JoinType {
  val joinString: String = "left join"
}
case object LeftOuterJoin extends JoinType {
  val joinString: String = "left outer join"
}
case object RightJoin extends JoinType {
  val joinString: String = "right join"
}
case object RightOuterJoin extends JoinType {
  val joinString: String = "right outer join"
}
case object FullJoin extends JoinType {
  val joinString: String = "full join"
}
case object FullOuterJoin extends JoinType {
  val joinString: String = "full outer join"
}
case object CrossJoin extends JoinType {
  val joinString: String = "cross join"
}

trait JoinNodeBuilder {
  def apply(lhs: Operand, rhs: Operand, joinType: JoinType): JoinNode
}
trait JoinNode extends BinaryOperator[Operand, Operand] {
  def lhs: Operand
  def rhs: Operand
  def joinType: JoinType
}

case class DefaultJoinNode(lhs: Operand, rhs: Operand, joinType: JoinType) extends JoinNode {
  val first: Operand = lhs
  val second: Operand = rhs
  val sqlString: String = s"${lhs.sqlString} ${joinType.joinString} ${rhs.sqlString}" 
}

sealed trait UnionType {
  def unionString: String
}
case object UnionDefault extends UnionType {
  val unionString: String = "union"
}
case object UnionAll extends UnionType {
  val unionString: String = "union all"
}
case object UnionDistinct extends UnionType {
  val unionString: String = "union distinct"
}

trait UnionNodeBuilder {
  def apply(operands: Seq[Operand], unionType: UnionType): UnionNode
}
trait UnionNode extends DQLNode {
  def operands: Seq[Operand]
  def unionType: UnionType
  def union(other: Operand): UnionNode
}

case class DefaultUnionNode(operands: Seq[Operand], unionType: UnionType) extends UnionNode {
  val sqlString: String = s"${operands.map(_.sqlString).mkString(" " + unionType.unionString + " ")}"
  def union(other: Operand): UnionNode = copy(operands = operands :+ other)
}

case class InsertInto(into: Operand, columns: Seq[Operand]) {
  def values(operands: Operand*)(implicit b: InsertNodeBuilder): InsertNode = b(into, columns, operands)
  def query(q: QueryNode)(implicit b: InsertQueryNodeBuilder): InsertQueryNode = b(into, columns, q)
}

trait InsertNodeBuilder {
  def apply(into: Operand, columns: Seq[Operand], values: Seq[Operand]): InsertNode
}

trait InsertNode extends DMLNode {
  def into: Operand
  def columns: Seq[Operand]
  def values: Seq[Operand]
}

case class DefaultInsertNode(into: Operand, columns: Seq[Operand], values: Seq[Operand]) extends InsertNode {
  val operands: Seq[Operand] = Seq(into) ++ columns ++ values
  val sqlString: String =
    if (columns.isEmpty)
      s"insert into ${into.sqlString} values (${values.map(_.sqlString).mkString(",")})"
    else
      s"insert into ${into.sqlString}(${columns.map(_.sqlString).mkString(",")}) values (${values.map(_.sqlString).mkString(",")})"
}

trait InsertQueryNodeBuilder {
  def apply(into: Operand, columns: Seq[Operand], query: QueryNode): InsertQueryNode
}

trait InsertQueryNode extends DMLNode {
  def into: Operand

  def columns: Seq[Operand]

  def query: QueryNode
}

case class DefaultInsertQueryNode(into: Operand, columns: Seq[Operand], query: QueryNode) extends InsertQueryNode {
  val operands: Seq[Operand] = into +: columns :+ query
  val sqlString: String =
    if (columns.isEmpty)
      s"insert into ${into.sqlString} ${query.sqlString}"
    else
      s"insert into ${into.sqlString}(${columns.map(_.sqlString).mkString(",")}) ${query.sqlString}"
}

trait DeleteFromNodeBuilder {
  def apply(table: Operand): DeleteFromNode
}

trait DeleteFromNode extends DMLNode {
  def table: Operand
  def where(operator: Operator)(implicit db: DeleteNodeBuilder, wb: WhereNodeBuilder): DeleteNode = db(table, wb(operator))
}

case class DefaultDeleteFromNode(table: Operand) extends DeleteFromNode {
  val operands: Seq[Operand] = Seq(table)
  val sqlString: String = s"delete from ${table.sqlString}"
}

trait DeleteNodeBuilder {
  def apply(table: Operand, where: WhereNode): DeleteNode
}
trait DeleteNode extends DMLNode

case class DefaultDeleteNode(table: Operand, where: WhereNode) extends DeleteNode {
  val operands: Seq[Operand] = Seq(table, where)
  val sqlString: String = s"delete from ${table.sqlString} ${where.sqlString}"
}

trait UpdatePairNodeBuilder {
  def apply(lhs: Operand, rhs: Operand): UpdatePairNode
}
trait UpdatePairNode extends Operator {
  def lhs: Operand
  def rhs: Operand
}

case class DefaultUpdatePairNode(lhs: Operand, rhs: Operand) extends UpdatePairNode {
  val operands: Seq[Operand] = Seq(lhs, rhs)
  val sqlString: String = s"set ${lhs.sqlString} = ${rhs.sqlString}"
}

case class Update(table: Operand) {
  def set(lhs: Operand, rhs: Operand)(implicit unb: UpdateNodeBuilder, upb: UpdatePairNodeBuilder): UpdateNode =
    unb(table, Seq(upb(lhs, rhs)))
}


trait UpdateNodeBuilder {
  def apply(table: Operand, pairs: Seq[UpdatePairNode]): UpdateNode
}
trait UpdateNode extends DMLNode {
  def table: Operand
  def pairs: Seq[UpdatePairNode]
  def set(lhs: Operand, rhs: Operand)(implicit unp: UpdateNodeBuilder, upb: UpdatePairNodeBuilder): UpdateNode =
    unp(table, pairs :+ upb(lhs, rhs))
  def where(operator: Operator)(implicit ub: UpdateWhereNodeBuilder, wb: WhereNodeBuilder): UpdateWhereNode =
    ub(this, wb(operator))
}

case class DefaultUpdateNode(table: Operand, pairs: Seq[UpdatePairNode]) extends UpdateNode {
  val operands: Seq[Operand] = Seq(table) ++ pairs
  val sqlString: String = s"update ${table.sqlString} ${pairs.map(_.sqlString).mkString(",")}"
}

trait UpdateWhereNodeBuilder {
  def apply(node: UpdateNode, where: WhereNode): UpdateWhereNode
}

trait UpdateWhereNode extends DMLNode {
  def node: UpdateNode
  def where: WhereNode
}

case class DefaultUpdateWhereNode(node: UpdateNode, where: WhereNode) extends UpdateWhereNode {
  val operands: Seq[Operand] = Seq(node, where)
  val sqlString: String = s"${node.sqlString} ${where.sqlString}"
}

trait CreateSchemaNodeBuilder {
  def apply(id: IdentifierNode): CreateSchemaNode
}
trait CreateSchemaNode extends DDLNode {
  def id: IdentifierNode
}

case class DefaultCreateSchemaNode(id: IdentifierNode) extends CreateSchemaNode {
  val operands = Seq(id)
  val sqlString = s"create schema ${id.sqlString}"
}

trait CreateTableNodeBuilder {
  def apply[E](model: Model[E])(implicit dst: SqlTypes, tb: TableIdentifierNodeBuilder): CreateTableNode[E]
}
trait CreateTableNode[E] extends DDLNode {
  def model: Model[E]
}


case class DefaultCreateTableNode[E](model: Model[E])
                                    (implicit dst: SqlTypes,
                                     tb: TableIdentifierNodeBuilder) extends CreateTableNode[E] {
  val operands: Seq[Operand] = Seq(tb(model))
  private val prefixed = model.sqlSchema.fold(model.sqlName)(schema => s"$schema.${model.sqlName}")
  def nullability(p: Property[_]) =
    if (p.optional) "" else " not null"
  def column(p: Property[_]) =
    s"${p.sqlName} ${p.sqlType}${nullability(p)}"
  private val body =
    if(model.properties.isEmpty)
      ""
    else
      s"(${model.properties.map(column).mkString(",")})"
  val sqlString: String = s"""create table $prefixed$body"""
}

trait PrimaryKeyNodeBuilder {
  def apply[E](model: Model[E], primaryKey: SqlPrimaryKey)(implicit tb: TableIdentifierNodeBuilder): PrimaryKeyNode[E]
}
trait PrimaryKeyNode[E] extends DDLNode {
  def model: Model[E]
  def primaryKey: SqlPrimaryKey
}

case class DefaultPrimaryKeyNode[E](model: Model[E], primaryKey: SqlPrimaryKey)
                                   (implicit tb: TableIdentifierNodeBuilder) extends PrimaryKeyNode[E] {
  val operands: Seq[Operand] = Seq(tb(model))
  private val prefixed = model.sqlSchema.fold(model.sqlName)(schema => s"$schema.${model.sqlName}")
  private val named = primaryKey.name.fold("primary key")(n => s"constraint $n primary key")
  val sqlString: String =  s"alter table $prefixed add $named (${primaryKey.properties.map(_.sqlName).mkString(",")})"
}

trait IndexNodeBuilder {
  def apply[E](model: Model[E], index: SqlIndex)(implicit tb: TableIdentifierNodeBuilder): IndexNode[E]
}
trait IndexNode[E] extends DDLNode {
  def model: Model[E]
  def index: SqlIndex
}

case class DefaultIndexNode[E](model: Model[E], index: SqlIndex)
                              (implicit tb: TableIdentifierNodeBuilder) extends IndexNode[E] {
  val operands: Seq[Operand] = Seq(tb(model))
  //private val prefixed = model.sqlSchema.fold(model.sqlName)(schema => s"$schema.${model.sqlName}")
  private val named = index.name.fold("index")(n => s"index $n")
  private val unique = if(index.unique) " unique" else ""
  val sqlString: String =  s"create$unique $named on ${model.sqlQualifiedName} (${index.properties.map(_.sqlName).mkString(",")})"
}

trait ForeignKeyNodeBuilder {
  def apply[E, R](model: Model[E], foreignKey: SqlForeignKey[R])(implicit tb: TableIdentifierNodeBuilder): ForeignKeyNode[E, R]
}
trait ForeignKeyNode[E, R] extends DDLNode {
  def model: Model[E]
  def foreignKey: SqlForeignKey[R]
}

case class DefaultForeignKeyNode[E, R](model: Model[E], foreignKey: SqlForeignKey[R])
                                   (implicit tb: TableIdentifierNodeBuilder) extends ForeignKeyNode[E, R] {
  val operands: Seq[Operand] = Seq(tb(model))
  //private val prefixed = model.sqlSchema.fold(model.sqlName)(schema => s"$schema.${model.sqlName}")
  private val named = foreignKey.name.fold("foreign key")(n => s"constraint $n foreign key")
  private val keys = s"(${foreignKey.keys.map(_.sqlName).mkString(",")})"
  private val references = s"(${foreignKey.references.map(_.sqlName).mkString(",")}) "
  val sqlString: String =  s"alter table ${model.sqlQualifiedName} add $named $keys references ${foreignKey.refModel.sqlQualifiedName}$references"
}



