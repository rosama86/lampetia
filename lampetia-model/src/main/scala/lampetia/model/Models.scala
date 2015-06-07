package lampetia.model

/**
 * @author Hossam Karim
 */

trait Models {

  def defaultSqlType: String = "varchar"

  case class CProperty[+A](name: String, features: Seq[Feature]) extends Property[A] {
    def set(feature: Feature): CProperty[A] = copy(features = features :+ feature)
  }

  def property[A](name: String): Property[A] = CProperty(name, Seq.empty[Feature])

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
  }

  implicit class ModelFeatures[A](model: Model[A]) {
    def sqlName: String = model.features.collectFirst {
      case SqlFeature.SqlName(value) => value
    }.getOrElse(model.name)
  }

  implicit class PropertyFeatures[A](p: Property[A]) {
    def sqlName: String = p.features.collectFirst {
      case SqlFeature.SqlName(value) => value
    }.getOrElse(p.name)

    def sqlType: String = p.features.collectFirst {
      case SqlFeature.SqlType(value) => value
    }.getOrElse(p.name)

    def jsonName: String = p.features.collectFirst {
      case JsonFeature.JsonName(value) => value
    }.getOrElse(p.name)
  }

}

object Models extends Models
