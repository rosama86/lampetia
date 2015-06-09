package lampetia.sql.dialect.h2

import lampetia.model.sql.DefaultSqlType

/**
 * @author Hossam Karim
 */


trait H2Dsl {

  implicit val h2DefaultSqlType: DefaultSqlType = new DefaultSqlType {
    def name: String = "varchar"
  }

}
