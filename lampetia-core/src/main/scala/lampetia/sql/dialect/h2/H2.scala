package lampetia.sql.dialect.h2

import lampetia.model._
import lampetia.model.sql.SqlTypes
import lampetia.sql.{SqlCodec, Ops, SqlIO}

/**
 * @author Hossam Karim
 */
trait H2
  extends H2Dsl
  with    SqlIO
  with    SqlCodec
  with    Ops {

  implicit val sqlTypes: SqlTypes = new SqlTypes {

    def name(propertyType: PropertyType[_]): String = propertyType match {
      case IntProperty => "integer"
      case FloatProperty => "float"
      case DoubleProperty => "double"
      case LongProperty => "long"
      case StringProperty => "varchar"
      case DateProperty  => "date"
      case DefaultProperty => "text"
    }
  }

}
