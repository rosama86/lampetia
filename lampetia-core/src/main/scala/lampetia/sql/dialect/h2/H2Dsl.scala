package lampetia.sql.dialect.h2

import lampetia.model.sql.DefaultSqlType
import lampetia.sql.ast._
import lampetia.sql.dialect.Dialect

/**
 * @author Hossam Karim
 */


trait H2Dsl extends Dsl with Dialect {

  implicit val defaultSqlType: DefaultSqlType = new DefaultSqlType {
    def name: String = "varchar"
  }

  implicit lazy val QueryNodeBuilder: QueryNodeBuilder = new QueryNodeBuilder {
    type N = QueryNode
    def apply(operands: Seq[Operand]): N = DefaultQueryNode(operands)
  }

}
