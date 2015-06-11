package lampetia.sql.dialect

import lampetia.model.sql.{DefaultSqlType, SqlForeignKey, SqlIndex, SqlPrimaryKey}
import lampetia.model.{Model, Property}
import lampetia.sql.ast._

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
    def apply[A](property: Property[A]): ColumnIdentifierNode[A] = DefaultColumnIdentifierNode(property)
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
  implicit def PrimaryKeyNodeBuilder: PrimaryKeyNodeBuilder = new PrimaryKeyNodeBuilder {
    def apply[E](model: Model[E], primaryKey: SqlPrimaryKey)(implicit tb: TableIdentifierNodeBuilder): PrimaryKeyNode[E] =
      DefaultPrimaryKeyNode(model, primaryKey)(tb)
  }
  implicit def IndexNodeBuilder: IndexNodeBuilder = new IndexNodeBuilder {
    def apply[E](model: Model[E], index: SqlIndex)(implicit tb: TableIdentifierNodeBuilder): IndexNode[E] =
      DefaultIndexNode(model, index)(tb)
  }
  implicit def ForeignKeyNodeBuilder: ForeignKeyNodeBuilder = new ForeignKeyNodeBuilder {
    def apply[E, R](model: Model[E], foreignKey: SqlForeignKey[R])(implicit tb: TableIdentifierNodeBuilder): ForeignKeyNode[E, R] =
      DefaultForeignKeyNode(model, foreignKey)(tb)
  }
}
