package lampetia.sql.dialect

import lampetia.meta.{Property, Model}
import lampetia.meta.feature.sql.{SqlTypes, SqlForeignKey, SqlIndex, SqlPrimaryKey}
import lampetia.sql.ast._

/**
 * @author Hossam Karim
 */

trait Dialect {

  implicit lazy val StringLiteralNodeBuilder: StringLiteralNodeBuilder = new StringLiteralNodeBuilder {
    type N = StringLiteralNode
    def apply(value: String): N = DefaultStringLiteralNode(value)
  }
  implicit lazy val IntegerLiteralNodeBuilder: IntegerLiteralNodeBuilder = new IntegerLiteralNodeBuilder {
    def apply(value: Int): IntegerLiteralNode = DefaultIntegerLiteralNode(value)
  }
  implicit lazy val IdentifierNodeBuilder: IdentifierNodeBuilder = new IdentifierNodeBuilder {
    def apply(name: String): IdentifierNode = DefaultIdentifierNode(name)
  }
  implicit lazy val TableIdentifierNodeBuilder: TableIdentifierNodeBuilder = new TableIdentifierNodeBuilder {
    def apply[A](model: Model[A]): TableIdentifierNode[A] = DefaultTableIdentifierNode(model)
  }
  implicit lazy val ColumnIdentifierNodeBuilder: ColumnIdentifierNodeBuilder = new ColumnIdentifierNodeBuilder {
    def apply[A](property: Property[A]): ColumnIdentifierNode[A] = DefaultColumnIdentifierNode(property)
  }
  implicit lazy val ParameterNodeBuilder: ParameterNodeBuilder = new ParameterNodeBuilder {
    def apply: ParameterNode = DefaultParameterNode
  }
  implicit lazy val NamedParameterNodeBuilder: NamedParameterNodeBuilder = new NamedParameterNodeBuilder {
    def apply(parameterName: String): NamedParameterNode = DefaultNamedParameterNode(parameterName)
  }
  implicit lazy val InfixNodeBuilder: InfixNodeBuilder = new InfixNodeBuilder {
    def apply(symbol: String, first: Operand, second: Operand, groupSecond: Boolean): InfixNode =
      DefaultInfixNode(symbol, first, second, groupSecond)
  }
  implicit lazy val PrefixNodeBuilder: PrefixNodeBuilder = new PrefixNodeBuilder {
    def apply(symbol: String, operands: Seq[Operand]): PrefixNode = DefaultPrefixNode(symbol, operands)
  }
  implicit lazy val PostfixNodeBuilder: PostfixNodeBuilder = new PostfixNodeBuilder {
    def apply(symbol: String, operands: Seq[Operand]): PostfixNode = DefaultPostfixNode(symbol, operands)
  }
  implicit lazy val SurroundNodeBuilder: SurroundNodeBuilder = new SurroundNodeBuilder {
    def apply(operand: Operand): SurroundNode = DefaultSurroundNode(operand)
  }
  implicit lazy val TypeNodeBuilder: TypeNodeBuilder = new TypeNodeBuilder {
    def apply(typeName: String): TypeNode = DefaultTypeNode(typeName)
  }
  implicit lazy val BetweenNodeBuilder: BetweenNodeBuilder = new BetweenNodeBuilder {
    def apply(lhs: Operand, first: Operand, second: Operand): BetweenNode = DefaultBetweenNode(lhs, first, second)
  }
  implicit lazy val AndNodeBuilder: AndNodeBuilder = new AndNodeBuilder {
    def apply(operands: Seq[Operand]): AndNode = DefaultAndNode(operands)
  }
  implicit lazy val OrNodeBuilder: OrNodeBuilder = new OrNodeBuilder {
    def apply(operands: Seq[Operand]): OrNode = DefaultOrNode(operands)
  }
  implicit lazy val NotNodeBuilder: NotNodeBuilder = new NotNodeBuilder {
    def apply(operand: Operand): NotNode = DefaultNotNode(operand)
  }
  implicit lazy val FunctionNodeBuilder: FunctionNodeBuilder = new FunctionNodeBuilder {
    def apply(name: String, operands: Seq[Operand]): FunctionNode = DefaultFunctionNode(name, operands)
  }

  implicit lazy val SelectNodeBuilder: SelectNodeBuilder = new SelectNodeBuilder {
    def apply(operands: Seq[Operand], distinct: Boolean = false): SelectNode = DefaultSelectNode(operands, distinct)
  }
  implicit lazy val FromNodeBuilder: FromNodeBuilder = new FromNodeBuilder {
    def apply(operands: Seq[Operand]): FromNode = DefaultFromNode(operands)
  }
  implicit lazy val WhereNodeBuilder: WhereNodeBuilder = new WhereNodeBuilder {
    def apply(operand: Operand): WhereNode = DefaultWhereNode(operand)
  }
  implicit lazy val JoinNodeBuilder: JoinNodeBuilder = new JoinNodeBuilder {
    def apply(lhs: Operand, rhs: Operand, joinType: JoinType): JoinNode = DefaultJoinNode(lhs, rhs, joinType)
  }
  implicit lazy val UnionNodeBuilder: UnionNodeBuilder = new UnionNodeBuilder {
    def apply(operands: Seq[Operand], unionType: UnionType): UnionNode = DefaultUnionNode(operands, unionType)
  }
  implicit lazy val InsertNodeBuilder: InsertNodeBuilder = new InsertNodeBuilder {
    def apply(into: Operand, columns: Seq[Operand], values: Seq[Operand]): InsertNode =
      DefaultInsertNode(into, columns, values)
  }
  implicit lazy val InsertQueryNodeBuilder: InsertQueryNodeBuilder = new InsertQueryNodeBuilder {
    def apply(into: Operand, columns: Seq[Operand], query: QueryNode): InsertQueryNode =
      DefaultInsertQueryNode(into, columns, query)
  }
  implicit lazy val DeleteFromNodeBuilder: DeleteFromNodeBuilder = new DeleteFromNodeBuilder {
    def apply(table: Operand): DeleteFromNode = DefaultDeleteFromNode(table)
  }
  implicit lazy val DeleteNodeBuilder: DeleteNodeBuilder = new DeleteNodeBuilder {
    def apply(table: Operand, where: WhereNode): DeleteNode = DefaultDeleteNode(table, where)
  }
  implicit lazy val UpdatePairNodeBuilder: UpdatePairNodeBuilder = new UpdatePairNodeBuilder {
    def apply(lhs: Operand, rhs: Operand): UpdatePairNode = DefaultUpdatePairNode(lhs, rhs)
  }
  implicit lazy val UpdateNodeBuilder: UpdateNodeBuilder = new UpdateNodeBuilder {
    def apply(table: Operand, pairs: Seq[UpdatePairNode]): UpdateNode = DefaultUpdateNode(table, pairs)
  }
  implicit lazy val UpdateWhereNodeBuilder: UpdateWhereNodeBuilder = new UpdateWhereNodeBuilder {
    def apply(node: UpdateNode, where: WhereNode): UpdateWhereNode = DefaultUpdateWhereNode(node, where)
  }
  implicit lazy val CreateSchemaNodeBuilder: CreateSchemaNodeBuilder = new CreateSchemaNodeBuilder {
    def apply(id: IdentifierNode): CreateSchemaNode = DefaultCreateSchemaNode(id)
  }
  implicit lazy val CreateTableNodeBuilder: CreateTableNodeBuilder = new CreateTableNodeBuilder {
    def apply[E](model: Model[E])(implicit dst: SqlTypes, tb: TableIdentifierNodeBuilder): CreateTableNode[E] =
      DefaultCreateTableNode(model)(dst, tb)
  }
  implicit lazy val PrimaryKeyNodeBuilder: PrimaryKeyNodeBuilder = new PrimaryKeyNodeBuilder {
    def apply[E](model: Model[E], primaryKey: SqlPrimaryKey)(implicit tb: TableIdentifierNodeBuilder): PrimaryKeyNode[E] =
      DefaultPrimaryKeyNode(model, primaryKey)(tb)
  }
  implicit lazy val IndexNodeBuilder: IndexNodeBuilder = new IndexNodeBuilder {
    def apply[E](model: Model[E], index: SqlIndex)(implicit tb: TableIdentifierNodeBuilder): IndexNode[E] =
      DefaultIndexNode(model, index)(tb)
  }
  implicit lazy val ForeignKeyNodeBuilder: ForeignKeyNodeBuilder = new ForeignKeyNodeBuilder {
    def apply[E, R](model: Model[E], foreignKey: SqlForeignKey[R])(implicit tb: TableIdentifierNodeBuilder): ForeignKeyNode[E, R] =
      DefaultForeignKeyNode(model, foreignKey)(tb)
  }
}
