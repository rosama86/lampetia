package lampetia.sql.ast

import lampetia.model.{Model, Property}

import scala.language.implicitConversions

/**
 * @author Hossam Karim
 */

trait Dsl {

  def ? = ParameterNode
  def surround(operand: Operand) = SurroundNode(operand)
  def typeName(typeName: String) = TypeNode(typeName)
  def select(operands: Operand*) = QueryNode(Seq(SelectNode(operands)))

  def not(operand: Operator) = NotNode(operand)
  def insertInto(operand: Operand, columns: Operand*) = InsertInto(operand, columns)
  def deleteFrom(operand: Operand) = DeleteFromNode(operand)
  def update(operand: Operand) = Update(operand)
  def function(name: String, operands: Operand*) = FunctionNode(name, operands)
  def jsonBuildObject(operands: Operand*) = FunctionNode("json_build_object", operands)
  def coalesce(operands: Operand*) = FunctionNode("coalesce", operands)

  trait StringsDsl extends Any {
    def value: String
    def ? : NamedParameterNode = NamedParameterNode(value)
    def literal: StringLiteralNode = StringLiteralNode(value)
    def identifier: IdentifierNode = IdentifierNode(value)
    def typeName: TypeNode = TypeNode(value)
  }

  trait IntegersDsl extends Any {
    def value: Int
    def literal: IntegerLiteralNode = IntegerLiteralNode(value)
  }

  trait OperandOps extends Any {
    def value: Operand
    def surround: SurroundNode = SurroundNode(value)
    def dot[B](other: B)(implicit c: B => Operand): InfixNode = InfixNode(".", value, other)
    def as[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" as ", value, other)
    def cast(other: TypeNode): InfixNode = InfixNode("::", value, other)
    def asc: PostfixNode = PostfixNode("asc", Seq(value))
    def desc: PostfixNode = PostfixNode("desc", Seq(value))
    def concat[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" || ", value, other)
    def ===[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" = ", value, other)
    def =!=[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" <> ", value, other)
    def <[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" < ", value, other)
    def <=[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" <= ", value, other)
    def >[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" > ", value, other)
    def >=[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" >= ", value, other)
    def isNull: PostfixNode = PostfixNode("is null", Seq(value))
    def isNotNull: PostfixNode = PostfixNode("is not null", Seq(value))
    def &[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" & ", value, other)
    def |[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" | ", value, other)
    def like[B](other: B)(implicit ev: B => Operand): InfixNode = InfixNode(" like ", value, other)
    def join[B](other: B)(implicit ev: B => Operand): JoinNode = JoinNode(value, other, NaturalJoin)
    def innerJoin[B](other: B)(implicit ev: B => Operand): JoinNode = JoinNode(value, other, InnerJoin)
    def leftJoin[B](other: B)(implicit ev: B => Operand): JoinNode = JoinNode(value, other, LeftJoin)
    def leftOuterJoin[B](other: B)(implicit ev: B => Operand): JoinNode = JoinNode(value, other, LeftOuterJoin)
    def rightJoin[B](other: B)(implicit ev: B => Operand): JoinNode = JoinNode(value, other, RightJoin)
    def rightOuterJoin[B](other: B)(implicit ev: B => Operand): JoinNode = JoinNode(value, other, RightOuterJoin)
    def fullJoin[B](other: B)(implicit ev: B => Operand): JoinNode = JoinNode(value, other, FullJoin)
    def fullOuterJoin[B](other: B)(implicit ev: B => Operand): JoinNode = JoinNode(value, other, FullOuterJoin)
    def crossJoin[B](other: B)(implicit ev: B => Operand): JoinNode = JoinNode(value, other, CrossJoin)
    def union[B](other: B)(implicit ev: B => Operator): UnionNode = UnionNode(Seq(value, other), UnionDefault)
    def unionAll[B](other: B)(implicit ev: B => Operator): UnionNode = UnionNode(Seq(value, other), UnionAll)
    def unionDistinct[B](other: B)(implicit ev: B => Operator): UnionNode = UnionNode(Seq(value, other), UnionDistinct)
    def on[B](other: B)(implicit ev: B => Operator): InfixNode = InfixNode(" on ", value, other, groupSecond = true)
    def between[B <: Operand, C <: Operand](b: B, c: C) = BetweenNode(value, b, c)
  }

  trait OperatorOps extends Any with OperandOps {
    override def value: Operator
    def and[B](other: B)(implicit ev: B => Operator): AndNode = AndNode(Seq(value, other))
    def or[B](other: B)(implicit ev: B => Operator): OrNode = OrNode(Seq(value, other))
  }


  implicit def symbolIdentifier(value: Symbol): IdentifierNode = IdentifierNode(value.name)

  trait SymbolsDsl extends Any with OperandOps {
    def symbol: Symbol
    def value: IdentifierNode = IdentifierNode(symbol.name)
    def ? : NamedParameterNode = NamedParameterNode(symbol.name)
  }

  implicit def liftModel[A](model: Model[A]): TableIdentifierNode[A] = TableIdentifierNode(model)

  implicit def liftProperty[A](property: Property[A]): ColumnIdentifierNode[A] = ColumnIdentifierNode(property)

  implicit def liftProperties(properties: Seq[Property[_]]): Seq[ColumnIdentifierNode[_]] =
    properties.map(p => ColumnIdentifierNode(p))

  trait PropertyLifterDsl[A] extends Any with OperandOps {
    def property: Property[A]
    def value: ColumnIdentifierNode[A] = ColumnIdentifierNode[A](property)
  }


}
