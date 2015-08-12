package lampetia.format

import lampetia.model._
import play.api.libs.json._

/**
 * @author Hossam Karim
 */

trait JsonFormat {

  def valueTypeFormat[A](read: String => A)(write: A => String): Format[A] = Format[A] (
    Reads[A](_.validate[String].map(read)),
    Writes[A](v => JsString(write(v)))
  )

  implicit lazy val codeJsonFormat: Format[Code] =
    valueTypeFormat[Code](Code)(_.value)

  implicit lazy val emailJsonFormat: Format[Email] =
    valueTypeFormat[Email](Email)(_.value)

  implicit lazy val resourceIdJsonFormat: Format[ResourceId] =
    valueTypeFormat[ResourceId](ResourceId)(_.value)

  implicit lazy val resourceTypeFormat: Format[ResourceType] =
    valueTypeFormat[ResourceType](ResourceType)(_.value)



}
