package lampetia.sql.ast

import lampetia.model.{Model, Property}
import lampetia.model.sql._

import scala.language.implicitConversions

/**
 * @author Hossam Karim
 */

trait Node {
  def sqlString: String
}

trait Operand extends Node

trait Operator extends Operand {
  def operands: Seq[Operand]
}

trait UnaryOperator[O <: Operand] extends Operator {
  def operand: O
  def operands: Seq[Operand] = Seq(operand)
}

trait BinaryOperator[A <: Operand, B <: Operand] extends Operator {
  def first: A
  def second: B
  def operands: Seq[Operand] = Seq(first, second)
}

trait DQLNode extends Operator

trait DMLNode extends Operator

trait LiteralNode extends Operand

case class StringLiteralNode(value: String) extends LiteralNode {
  def sqlString: String = s"'$value'"
}

case class IntegerLiteralNode(value: Integer) extends LiteralNode {
  def sqlString: String = s"$value"
}

case class IdentifierNode(name: String) extends Operand {
  val sqlString: String = name
}

case class TableIdentifierNode[A](model: Model[A]) extends Operand {
  val sqlString: String = model.sqlName
  def as(alias: IdentifierNode) = InfixNode(" as ", this, alias)
  def join[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" join ", this, other)
  def innerJoin[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" inner join ", this, other)
  def leftJoin[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" left join ", this, other)
  def rightJoin[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" right join ", this, other)
  def leftOuterJoin[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" left outer join ", this, other)
}

case class ColumnIdentifierNode[E, A](property: Property[E, A]) extends Operand {
  val sqlString: String = property.sqlName
}

case object ParameterNode extends Operand {
  val sqlString: String = "?"
}

case class NamedParameterNode(parameterName: String) extends Operand {
  val sqlString: String = s"#{$parameterName}"
}

case class InfixNode(symbol: String, first: Operand, second: Operand, groupSecond: Boolean = false) extends BinaryOperator[Operand, Operand] {
  def sqlString: String =
    if (groupSecond)
      s"${first.sqlString}$symbol(${second.sqlString})"
    else
      s"${first.sqlString}$symbol${second.sqlString}"
}

case class PrefixNode(symbol: String, operands: Seq[Operand]) extends Operator {
  def sqlString: String = s"$symbol ${operands.map(_.sqlString).mkString(",")}"
}

case class PostfixNode(symbol: String, operands: Seq[Operand]) extends Operator {
  def sqlString: String = s"${operands.map(_.sqlString).mkString(",")} $symbol"
}

case class SurroundNode(operand: Operand) extends Operator {
  def operands: Seq[Operand] = Seq(operand)
  def sqlString: String = s"(${operand.sqlString})"
}

case class TypeNode(typeName: String) extends Operand {
  def sqlString: String = typeName
}

case class BetweenNode(lhs: Operand, first: Operand, second: Operand) extends Operator {
  val operands: Seq[Operand] = Seq(lhs, first, second)
  val sqlString: String = s"${lhs.sqlString} between ${first.sqlString} and ${second.sqlString}"
}

case class AndNode(operands: Seq[Operand]) extends Operator {
  def sqlString: String = s"(${operands.map(_.sqlString).mkString(" and ")})"
  def and(other: Operand): AndNode = copy(operands = operands :+ other)
}

case class OrNode(operands: Seq[Operand]) extends Operator {
  def sqlString: String = s"(${operands.map(_.sqlString).mkString(" or ")})"
  def or(other: Operand): OrNode = copy(operands = operands :+ other)
}

case class NotNode(operand: Operand) extends UnaryOperator[Operand] {
  def sqlString: String = s"(not ${operand.sqlString})"
}

case class FunctionNode(name: String, operands: Seq[Operand]) extends Operator {
  def sqlString: String = s"$name(${operands.map(_.sqlString).mkString(",")})"
}


case class QueryNode(operands: Seq[Operand]) extends DQLNode {
  protected def append(operand: Operand): QueryNode = copy(operands = operands :+ operand)
  def sqlString: String = s"${operands.map(_.sqlString).mkString(" ")}"
  def select(operands: Operand*) = append(SelectNode(operands))
  def from(operands: Operand*) = append(FromNode(operands))
  def where(operand: Operand) = append(WhereNode(operand))
  def groupBy(operands: Operand*) = append(PrefixNode("group by", operands))
  def having(operands: Operand*) = append(PrefixNode("having", operands))
  def orderBy(operands: Operand*) = append(PrefixNode("order by", operands))
  def limit(operands: Operand*) = append(PrefixNode("limit", operands))
  def offset(operands: Operand*) = append(PrefixNode("offset", operands))
}
case class SelectNode(operands: Seq[Operand]) extends DQLNode {
  def sqlString: String = s"select ${operands.map(_.sqlString).mkString(",")}"
}

case class FromNode(operands: Seq[Operand]) extends DQLNode {
  def sqlString: String = s"from ${operands.map(_.sqlString).mkString(",")}"
}

case class WhereNode(operand: Operand) extends DQLNode with UnaryOperator[Operand] {
  def sqlString: String = s"where ${operands.map(_.sqlString).mkString(" ")}"
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

case class JoinNode(lhs: Operand, rhs: Operand, joinType: JoinType) extends BinaryOperator[Operand, Operand] {
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

case class UnionNode(operands: Seq[Operand], unionType: UnionType) extends DQLNode {
  val sqlString: String = s"${operands.map(_.sqlString).mkString(" " + unionType.unionString + " ")}"
  def union(other: Operand): UnionNode = copy(operands = operands :+ other)
}

case class InsertInto(into: Operand, columns: Seq[Operand]) {
  def values(operands: Operand*)= InsertNode(into, columns, operands)
}

case class InsertNode(into: Operand, columns: Seq[Operand], values: Seq[Operand]) extends DMLNode {
  def operands: Seq[Operand] = Seq(into) ++ columns ++ values
  def sqlString: String =
    if (columns.isEmpty)
      s"insert into ${into.sqlString} values (${values.map(_.sqlString).mkString(",")})"
    else
      s"insert into ${into.sqlString}(${columns.map(_.sqlString).mkString(",")}) values (${values.map(_.sqlString).mkString(",")})"
}

case class DeleteFromNode(table: Operand) extends DMLNode {
  def where(operator: Operator): DeleteNode = DeleteNode(table, WhereNode(operator))
  def operands: Seq[Operand] = Seq(table)
  def sqlString: String = s"delete from ${table.sqlString}"
}

case class DeleteNode(table: Operand, where: WhereNode) extends DMLNode {
  def operands: Seq[Operand] = Seq(table, where)
  def sqlString: String = s"delete from ${table.sqlString} ${where.sqlString}"
}

case class UpdatePair(lhs: Operand, rhs: Operand) extends Operator {
  def operands: Seq[Operand] = Seq(lhs, rhs)
  def sqlString: String = s"set ${lhs.sqlString} = ${rhs.sqlString}"
}

case class Update(table: Operand) {
  def set(lhs: Operand, rhs: Operand): UpdateNode = UpdateNode(table, Seq(UpdatePair(lhs, rhs)))
}

case class UpdateNode(table: Operand, pairs: Seq[UpdatePair]) extends DMLNode {
  def set(lhs: Operand, rhs: Operand): UpdateNode = copy(pairs = pairs :+ UpdatePair(lhs, rhs))
  def operands: Seq[Operand] = Seq(table) ++ pairs
  def sqlString: String = s"update ${table.sqlString} ${pairs.map(_.sqlString).mkString(",")}"
  def where(operator: Operator): UpdateWhereNode = UpdateWhereNode(this, WhereNode(operator))
}

case class UpdateWhereNode(node: UpdateNode, where: WhereNode) extends DMLNode {
  def operands: Seq[Operand] = Seq(node, where)
  def sqlString: String = s"${node.sqlString} ${where.sqlString}"
}
