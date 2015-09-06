package lampetia.format

import lampetia.model._
import play.api.libs.json._

/**
 * @author Hossam Karim
 */

trait JsonFormat {

  def stringValueTypeFormat[A](read: String => A)(write: A => String): Format[A] = Format[A] (
    Reads[A](_.validate[String].map(read)),
    Writes[A](v => JsString(write(v)))
  )

  def intValueTypeFormat[A](read: Int => A)(write: A => Int): Format[A] = Format[A] (
    Reads[A](_.validate[Int].map(read)),
    Writes[A](v => JsNumber(write(v)))
  )

  implicit lazy val codeJsonFormat: Format[Code] =
    stringValueTypeFormat[Code](Code)(_.value)

  implicit lazy val emailJsonFormat: Format[Email] =
    stringValueTypeFormat[Email](Email)(_.value)

  implicit lazy val resourceIdJsonFormat: Format[ResourceId] =
    stringValueTypeFormat[ResourceId](ResourceId)(_.value)

  implicit lazy val resourceTypeFormat: Format[ResourceUri] =
    stringValueTypeFormat[ResourceUri](ResourceUri)(_.value)



}
