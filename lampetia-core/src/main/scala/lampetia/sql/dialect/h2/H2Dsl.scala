package lampetia.sql.dialect.h2

import lampetia.model.sql.DefaultSqlType
import lampetia.sql.ast._

/**
 * @author Hossam Karim
 */


trait H2Dsl extends Dsl with Dialect {

  implicit val defaultSqlType: DefaultSqlType = new DefaultSqlType {
    def name: String = "varchar"
  }

}
