package lampetia.sql.ast

import lampetia.meta.{Property, Model}
import lampetia.meta.feature.sql.{SqlIndex, SqlForeignKey, SqlPrimaryKey, SqlTypes}
import scala.language.implicitConversions



trait Dsl {

  def Q = this

  def ?(implicit b: ParameterNodeBuilder): ParameterNode = b.apply
  def *(implicit b: IdentifierNodeBuilder): IdentifierNode = b("*")
  def surround(operand: Operand)(implicit b: SurroundNodeBuilder): SurroundNode = b(operand)
  def typeName(typeName: String)(implicit b: TypeNodeBuilder) = b(typeName)
  def select(operands: Operand*)(implicit qb: QueryNodeBuilder, sb: SelectNodeBuilder): qb.N = qb(Seq(sb(operands)))
  def selectDistinct(operands: Operand*)(implicit qb: QueryNodeBuilder, sb: SelectNodeBuilder): qb.N = qb(Seq(sb(operands, true)))

  def not(operand: Operator)(implicit b: NotNodeBuilder): NotNode = b(operand)
  def insertInto(operand: Operand, columns: Operand*): InsertInto = InsertInto(operand, columns)
  def deleteFrom(operand: Operand)(implicit b: DeleteFromNodeBuilder): DeleteFromNode = b(operand)
  def update(operand: Operand): Update = Update(operand)
  def function(name: String, operands: Operand*)(implicit b: FunctionNodeBuilder): FunctionNode = b(name, operands)
  def createTable[E](model: Model[E])(implicit b: CreateTableNodeBuilder, dst: SqlTypes, tb: TableIdentifierNodeBuilder) = b(model)
  def primaryKey[E](model: Model[E], pk: SqlPrimaryKey)(implicit b: PrimaryKeyNodeBuilder, tb: TableIdentifierNodeBuilder) = b(model, pk)
  def foreignKey[E, R](model: Model[E], fk: SqlForeignKey[R])(implicit b: ForeignKeyNodeBuilder, tb: TableIdentifierNodeBuilder) = b(model, fk)
  def index[E](model: Model[E], index: SqlIndex)(implicit b: IndexNodeBuilder, tb: TableIdentifierNodeBuilder) = b(model, index)

  trait StringsDsl extends Any {
    def value: String
    def ?(implicit b: NamedParameterNodeBuilder) : NamedParameterNode = b(value)
    def literal(implicit b: StringLiteralNodeBuilder): StringLiteralNode = b(value)
    def identifier(implicit b: IdentifierNodeBuilder): IdentifierNode = b(value)
    def typeName(implicit b: TypeNodeBuilder): TypeNode = b(value)
  }

  trait IntegersDsl extends Any {
    def value: Int
    def literal(implicit b: IntegerLiteralNodeBuilder): IntegerLiteralNode = b(value)
  }


  trait OperandOps[V <: Operand] extends Any {
    def value: V
    def surround(implicit b: SurroundNodeBuilder): SurroundNode = b(value)
    def dot[B](other: B)(implicit c: B => Operand, b: InfixNodeBuilder): InfixNode = b(".", value, other)
    def as[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" as ", value, other)
    def cast(other: TypeNode)(implicit b: CastNodeBuilder): CastNode = b(value, other)
    def asc(implicit b: PostfixNodeBuilder): PostfixNode = b("asc", Seq(value))
    def desc(implicit b: PostfixNodeBuilder): PostfixNode = b("desc", Seq(value))
    def concat[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" || ", value, other)
    def ===[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" = ", value, other)
    def =!=[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" <> ", value, other)
    def <[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" < ", value, other)
    def <=[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" <= ", value, other)
    def >[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" > ", value, other)
    def >=[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" >= ", value, other)
    def in[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" in ", value, other, groupSecond = true)
    def isNull(implicit b: PostfixNodeBuilder): PostfixNode = b("is null", Seq(value))
    def isNotNull(implicit b: PostfixNodeBuilder): PostfixNode = b("is not null", Seq(value))
    def &[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" & ", value, other)
    def |[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" | ", value, other)
    def like[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" like ", value, other)
    def join[B](other: B)(implicit ev: B => Operand, b: JoinNodeBuilder): JoinNode = b(value, other, NaturalJoin)
    def innerJoin[B](other: B)(implicit ev: B => Operand, b: JoinNodeBuilder): JoinNode = b(value, other, InnerJoin)
    def leftJoin[B](other: B)(implicit ev: B => Operand, b: JoinNodeBuilder): JoinNode = b(value, other, LeftJoin)
    def leftOuterJoin[B](other: B)(implicit ev: B => Operand, b: JoinNodeBuilder): JoinNode = b(value, other, LeftOuterJoin)
    def rightJoin[B](other: B)(implicit ev: B => Operand, b: JoinNodeBuilder): JoinNode = b(value, other, RightJoin)
    def rightOuterJoin[B](other: B)(implicit ev: B => Operand, b: JoinNodeBuilder): JoinNode = b(value, other, RightOuterJoin)
    def fullJoin[B](other: B)(implicit ev: B => Operand, b: JoinNodeBuilder): JoinNode = b(value, other, FullJoin)
    def fullOuterJoin[B](other: B)(implicit ev: B => Operand, b: JoinNodeBuilder): JoinNode = b(value, other, FullOuterJoin)
    def crossJoin[B](other: B)(implicit ev: B => Operand, b: JoinNodeBuilder): JoinNode = b(value, other, CrossJoin)
    def union[B](other: B)(implicit ev: B => Operator, b: UnionNodeBuilder): UnionNode = b(Seq(value, other), UnionDefault)
    def unionAll[B](other: B)(implicit ev: B => Operator, b: UnionNodeBuilder): UnionNode = b(Seq(value, other), UnionAll)
    def unionDistinct[B](other: B)(implicit ev: B => Operator, b: UnionNodeBuilder): UnionNode = b(Seq(value, other), UnionDistinct)
    def on[B](other: B)(implicit ev: B => Operator, b: InfixNodeBuilder): InfixNode = b(" on ", value, other, groupSecond = true)
    def between[B <: Operand, C <: Operand](b: B, c: C)(implicit bnb: BetweenNodeBuilder) = bnb(value, b, c)
  }

  trait OperatorOps[V <: Operator] extends Any  { this: OperandOps[V] =>
    def value: V
    def and[B](other: B)(implicit ev: B => Operator, b: AndNodeBuilder): AndNode = b(Seq(value, other))
    def or[B](other: B)(implicit ev: B => Operator, b: OrNodeBuilder): OrNode = b(Seq(value, other))
  }


  implicit def symbolIdentifier(value: Symbol)(implicit b: IdentifierNodeBuilder): IdentifierNode = b(value.name)


  implicit def liftModel[A](model: Model[A])(implicit b: TableIdentifierNodeBuilder): TableIdentifierNode[A] =
    b(model)

  implicit def liftProperty[A](property: Property[A])(implicit b: ColumnIdentifierNodeBuilder): ColumnIdentifierNode[A] =
    b(property)

  implicit def liftProperties(properties: Seq[Property[_]])(implicit b: ColumnIdentifierNodeBuilder): Seq[ColumnIdentifierNode[_]] =
    properties.map(p => b(p))

}
