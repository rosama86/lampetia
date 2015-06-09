package lampetia.model

/**
 * @author Hossam Karim
 */

package object json {

  sealed trait JsonFeature extends Any with Feature

  case class JsonName(value: String) extends AnyVal with JsonFeature

  def name(value: String): JsonFeature = JsonName(value)

  implicit class PropertyFeatures[E, A](val p: Property[E, A]) extends AnyVal {

    def jsonName: String = p.features.collectFirst {
      case JsonName(value) => value
    }.getOrElse(p.name)


  }

}
