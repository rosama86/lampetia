package lampetia.model

/**
 * @author Hossam Karim
 */

package object json {

  sealed trait JsonFeature extends Any with Feature

  case class JsonName(value: String) extends AnyVal with JsonFeature

  def name(value: String): JsonFeature = JsonName(value)

  implicit class PropertyFeatures[A](val p: Property[A]) extends AnyVal {

    def features = p.features.reverse

    def jsonName: String = features.collectFirst {
      case JsonName(value) => value
    }.getOrElse(p.propertyName)


  }

}
