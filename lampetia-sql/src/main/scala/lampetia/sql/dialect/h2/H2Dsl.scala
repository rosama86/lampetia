package lampetia.sql.dialect.h2

import lampetia.model._
import lampetia.meta.feature.sql.SqlTypes
import lampetia.sql.ast._
import lampetia.sql.dialect.Dialect

/**
 * @author Hossam Karim
 */


trait H2Dsl extends Dsl with Dialect {



  implicit lazy val QueryNodeBuilder: QueryNodeBuilder = new QueryNodeBuilder {
    type N = QueryNode
    def apply(operands: Seq[Operand]): N = DefaultQueryNode(operands)
  }

}
