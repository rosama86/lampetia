package lampetia.model

/**
 * @author Hossam Karim
 */
package object sql {

  sealed trait SqlFeature extends Any with Feature

  case class SqlName(value: String) extends AnyVal with SqlFeature

  def name(value: String): SqlFeature = SqlName(value)

  case class SqlType(value: String) extends AnyVal with SqlFeature

  def `type`(value: String): SqlFeature = SqlType(value)

  case class SqlSchema(value: String) extends AnyVal with SqlFeature

  def schema(value: String): SqlSchema = SqlSchema(value)

  implicit class ModelFeatures[A](val model: Model[A]) extends AnyVal {

    def sqlName: String = model.features.collectFirst {
      case SqlName(value) => value
    }.getOrElse(model.name)

    def sqlSchema: Option[String] = model.features.collectFirst {
      case SqlSchema(value) => value
    }
  }

  implicit class PropertyFeatures[E, A](val p: Property[E, A]) {

    def sqlName: String = p.features.collectFirst {
      case SqlName(value) => value
    }.getOrElse(p.name)

    def sqlType(implicit dmt: DefaultSqlType): String = p.features.collectFirst {
      case SqlType(value) => value
    }.getOrElse(dmt.name)

  }

}
