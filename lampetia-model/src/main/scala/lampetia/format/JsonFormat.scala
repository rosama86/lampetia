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

  implicit lazy val nameJsonFormat: Format[Name] =
    stringValueTypeFormat[Name](Name)(_.value)

  implicit lazy val titleJsonFormat: Format[Title] =
    stringValueTypeFormat[Title](Title)(_.value)

  implicit lazy val urlJsonFormat: Format[Url] =
    stringValueTypeFormat[Url](Url)(_.value)

  implicit lazy val localeJsonFormat: Format[Locale] =
    stringValueTypeFormat[Locale](Locale)(_.value)

  implicit lazy val phoneJsonFormat: Format[Phone] =
    stringValueTypeFormat[Phone](Phone)(_.value)



}
