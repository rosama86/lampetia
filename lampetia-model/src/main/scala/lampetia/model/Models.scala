package lampetia.model


/**
 * @author Hossam Karim
 */


sealed trait JsonFeature extends Feature
sealed trait SqlFeature extends Feature

object JsonFeature {
  case class JsonName(value: String) extends JsonFeature
  def name(value: String): JsonFeature = JsonName(value)
}

object SqlFeature {
  case class SqlName(value: String) extends SqlFeature
  def name(value: String): SqlFeature = SqlName(value)
  case class SqlType(value: String) extends JsonFeature
  def `type`(value: String): JsonFeature = SqlType(value)
  case class SqlSchema(value: String) extends SqlFeature
  def schema(value: String): SqlSchema = SqlSchema(value)
}

trait Models {

  def defaultSqlType: String = "varchar"

  implicit class ModelFeatures[A](model: Model[A]) {

    def sqlName: String = model.features.collectFirst {
      case SqlFeature.SqlName(value) => value
    }.getOrElse(model.name)

    def sqlSchema: Option[String] = model.features.collectFirst {
      case SqlFeature.SqlSchema(value) => value
    }
  }

  implicit class PropertyFeatures[E, A](p: Property[E, A]) {

    def sqlName: String = p.features.collectFirst {
      case SqlFeature.SqlName(value) => value
    }.getOrElse(p.name)

    def sqlType: String = p.features.collectFirst {
      case SqlFeature.SqlType(value) => value
    }.getOrElse(defaultSqlType)

    def jsonName: String = p.features.collectFirst {
      case JsonFeature.JsonName(value) => value
    }.getOrElse(p.name)
  }

}

object Models extends Models
