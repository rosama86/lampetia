package lampetia.sql.ast

import lampetia.model.sql.DefaultSqlType
import lampetia.model.{Model, Property}

import scala.language.implicitConversions

/**
 * @author Hossam Karim
 */

trait Dialect {
  implicit def StringLiteralNodeBuilder: StringLiteralNodeBuilder = new StringLiteralNodeBuilder {
    def apply(value: String): StringLiteralNode = DefaultStringLiteralNode(value)
  }
  implicit def IntegerLiteralNodeBuilder: IntegerLiteralNodeBuilder = new IntegerLiteralNodeBuilder {
    def apply(value: Int): IntegerLiteralNode = DefaultIntegerLiteralNode(value)
  }
  implicit def IdentifierNodeBuilder: IdentifierNodeBuilder = new IdentifierNodeBuilder {
    def apply(name: String): IdentifierNode = DefaultIdentifierNode(name)
  }
  implicit def TableIdentifierNodeBuilder: TableIdentifierNodeBuilder = new TableIdentifierNodeBuilder {
    def apply[A](model: Model[A]): TableIdentifierNode[A] = DefaultTableIdentifierNode(model)
  }
  implicit def ColumnIdentifierNodeBuilder: ColumnIdentifierNodeBuilder = new ColumnIdentifierNodeBuilder {
    def apply[E, A](property: Property[E, A]): ColumnIdentifierNode[E, A] = DefaultColumnIdentifierNode(property)
  }
  implicit def ParameterNodeBuilder: ParameterNodeBuilder = new ParameterNodeBuilder {
    def apply: ParameterNode = DefaultParameterNode
  }
  implicit def NamedParameterNodeBuilder: NamedParameterNodeBuilder = new NamedParameterNodeBuilder {
    def apply(parameterName: String): NamedParameterNode = DefaultNamedParameterNode(parameterName)
  }
  implicit def InfixNodeBuilder: InfixNodeBuilder = new InfixNodeBuilder {
    def apply(symbol: String, first: Operand, second: Operand, groupSecond: Boolean): InfixNode =
      DefaultInfixNode(symbol, first, second, groupSecond)
  }
  implicit def PrefixNodeBuilder: PrefixNodeBuilder = new PrefixNodeBuilder {
    def apply(symbol: String, operands: Seq[Operand]): PrefixNode = DefaultPrefixNode(symbol, operands)
  }
  implicit def PostfixNodeBuilder: PostfixNodeBuilder = new PostfixNodeBuilder {
    def apply(symbol: String, operands: Seq[Operand]): PostfixNode = DefaultPostfixNode(symbol, operands)
  }
  implicit def SurroundNodeBuilder: SurroundNodeBuilder = new SurroundNodeBuilder {
    def apply(operand: Operand): SurroundNode = DefaultSurroundNode(operand)
  }
  implicit def TypeNodeBuilder: TypeNodeBuilder = new TypeNodeBuilder {
    def apply(typeName: String): TypeNode = DefaultTypeNode(typeName)
  }
  implicit def BetweenNodeBuilder: BetweenNodeBuilder = new BetweenNodeBuilder {
    def apply(lhs: Operand, first: Operand, second: Operand): BetweenNode = DefaultBetweenNode(lhs, first, second)
  }
  implicit def AndNodeBuilder: AndNodeBuilder = new AndNodeBuilder {
    def apply(operands: Seq[Operand]): AndNode = DefaultAndNode(operands)
  }
  implicit def OrNodeBuilder: OrNodeBuilder = new OrNodeBuilder {
    def apply(operands: Seq[Operand]): OrNode = DefaultOrNode(operands)
  }
  implicit def NotNodeBuilder: NotNodeBuilder = new NotNodeBuilder {
    def apply(operand: Operand): NotNode = DefaultNotNode(operand)
  }
  implicit def FunctionNodeBuilder: FunctionNodeBuilder = new FunctionNodeBuilder {
    def apply(name: String, operands: Seq[Operand]): FunctionNode = DefaultFunctionNode(name, operands)
  }
  implicit def QueryNodeBuilder: QueryNodeBuilder = new QueryNodeBuilder {
    def apply(operands: Seq[Operand]): QueryNode = DefaultQueryNode(operands)
  }
  implicit def SelectNodeBuilder: SelectNodeBuilder = new SelectNodeBuilder {
    def apply(operands: Seq[Operand]): SelectNode = DefaultSelectNode(operands)
  }
  implicit def FromNodeBuilder: FromNodeBuilder = new FromNodeBuilder {
    def apply(operands: Seq[Operand]): FromNode = DefaultFromNode(operands)
  }
  implicit def WhereNodeBuilder: WhereNodeBuilder = new WhereNodeBuilder {
    def apply(operand: Operand): WhereNode = DefaultWhereNode(operand)
  }
  implicit def JoinNodeBuilder: JoinNodeBuilder = new JoinNodeBuilder {
    def apply(lhs: Operand, rhs: Operand, joinType: JoinType): JoinNode = DefaultJoinNode(lhs, rhs, joinType)
  }
  implicit def UnionNodeBuilder: UnionNodeBuilder = new UnionNodeBuilder {
    def apply(operands: Seq[Operand], unionType: UnionType): UnionNode = DefaultUnionNode(operands, unionType)
  }
  implicit def InsertNodeBuilder: InsertNodeBuilder = new InsertNodeBuilder {
    def apply(into: Operand, columns: Seq[Operand], values: Seq[Operand]): InsertNode =
      DefaultInsertNode(into, columns, values)
  }
  implicit def DeleteFromNodeBuilder: DeleteFromNodeBuilder = new DeleteFromNodeBuilder {
    def apply(table: Operand): DeleteFromNode = DefaultDeleteFromNode(table)
  }
  implicit def DeleteNodeBuilder: DeleteNodeBuilder = new DeleteNodeBuilder {
    def apply(table: Operand, where: WhereNode): DeleteNode = DefaultDeleteNode(table, where)
  }
  implicit def UpdatePairNodeBuilder: UpdatePairNodeBuilder = new UpdatePairNodeBuilder {
    def apply(lhs: Operand, rhs: Operand): UpdatePairNode = DefaultUpdatePairNode(lhs, rhs)
  }
  implicit def UpdateNodeBuilder: UpdateNodeBuilder = new UpdateNodeBuilder {
    def apply(table: Operand, pairs: Seq[UpdatePairNode]): UpdateNode = DefaultUpdateNode(table, pairs)
  }
  implicit def UpdateWhereNodeBuilder: UpdateWhereNodeBuilder = new UpdateWhereNodeBuilder {
    def apply(node: UpdateNode, where: WhereNode): UpdateWhereNode = DefaultUpdateWhereNode(node, where)
  }
  implicit def CreateSchemaNodeBuilder: CreateSchemaNodeBuilder = new CreateSchemaNodeBuilder {
    def apply(id: IdentifierNode): CreateSchemaNode = DefaultCreateSchemaNode(id)
  }
  implicit def CreateTableNodeBuilder: CreateTableNodeBuilder = new CreateTableNodeBuilder {
    def apply[E](model: Model[E])(implicit dst: DefaultSqlType, tb: TableIdentifierNodeBuilder): CreateTableNode[E] =
      DefaultCreateTableNode(model)(dst, tb)
  }
}

trait Dsl {

  def Q = this

  def ?(implicit b: ParameterNodeBuilder): ParameterNode = b.apply
  def surround(operand: Operand)(implicit b: SurroundNodeBuilder): SurroundNode = b(operand)
  def typeName(typeName: String)(implicit b: TypeNodeBuilder) = b(typeName)
  def select(operands: Operand*)(implicit qb: QueryNodeBuilder, sb: SelectNodeBuilder) = qb(Seq(sb(operands)))

  def not(operand: Operator)(implicit b: NotNodeBuilder): NotNode = b(operand)
  def insertInto(operand: Operand, columns: Operand*): InsertInto = InsertInto(operand, columns)
  def deleteFrom(operand: Operand)(implicit b: DeleteFromNodeBuilder): DeleteFromNode = b(operand)
  def update(operand: Operand): Update = Update(operand)
  def function(name: String, operands: Operand*)(implicit b: FunctionNodeBuilder): FunctionNode = b(name, operands)
  def createTable[E](model: Model[E])(implicit b: CreateTableNodeBuilder, dst: DefaultSqlType, tb: TableIdentifierNodeBuilder) = b(model)

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

  trait OperandOps extends Any {
    def value: Operand
    def surround(implicit b: SurroundNodeBuilder): SurroundNode = b(value)
    def dot[B](other: B)(implicit c: B => Operand, b: InfixNodeBuilder): InfixNode = b(".", value, other)
    def as[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" as ", value, other)
    def cast(other: TypeNode, b: InfixNodeBuilder): InfixNode = b("::", value, other)
    def asc(implicit b: PostfixNodeBuilder): PostfixNode = b("asc", Seq(value))
    def desc(implicit b: PostfixNodeBuilder): PostfixNode = b("desc", Seq(value))
    def concat[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" || ", value, other)
    def ===[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" = ", value, other)
    def =!=[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" <> ", value, other)
    def <[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" < ", value, other)
    def <=[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" <= ", value, other)
    def >[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" > ", value, other)
    def >=[B](other: B)(implicit ev: B => Operand, b: InfixNodeBuilder): InfixNode = b(" >= ", value, other)
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

  trait OperatorOps extends Any with OperandOps {
    override def value: Operator
    def and[B](other: B)(implicit ev: B => Operator, b: AndNodeBuilder): AndNode = b(Seq(value, other))
    def or[B](other: B)(implicit ev: B => Operator, b: OrNodeBuilder): OrNode = b(Seq(value, other))
  }


  implicit def symbolIdentifier(value: Symbol)(implicit b: IdentifierNodeBuilder): IdentifierNode = b(value.name)

  trait SymbolsDsl extends Any with OperandOps {
    def symbol: Symbol
    def asIdentifier(implicit b: IdentifierNodeBuilder): IdentifierNode = b(symbol.name)
    def ?(implicit b: NamedParameterNodeBuilder): NamedParameterNode = b(symbol.name)
  }

  trait UpdateCoupleDsl[E, A] extends Any {
    def property: Property[E, A]
    def :=(operand: Operand)(implicit b: ColumnIdentifierNodeBuilder): (Operand, Operand) = liftProperty(property) -> operand
  }

  implicit def liftModel[A](model: Model[A])(implicit b: TableIdentifierNodeBuilder): TableIdentifierNode[A] =
    b(model)

  implicit def liftProperty[E, A](property: Property[E, A])(implicit b: ColumnIdentifierNodeBuilder): ColumnIdentifierNode[E, A] =
    b(property)

  implicit def liftProperties(properties: Seq[Property[_, _]])(implicit b: ColumnIdentifierNodeBuilder): Seq[ColumnIdentifierNode[_, _]] =
    properties.map(p => b(p))

  trait PropertyLifterDsl[E, A] extends Any with OperandOps {
    def property: Property[E, A]
    def asColumnIdentifier(implicit b: ColumnIdentifierNodeBuilder): ColumnIdentifierNode[E, A] = b[E, A](property)
  }


}
