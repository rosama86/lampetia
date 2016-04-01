package lampetia.example.format

import lampetia.model.util._
import lampetia.example.model._
import play.api.libs.json.Format
import play.api.libs.json._


/**
  * @author Hossam Karim
  */

trait JsonFormat {

  def stringValueTypeFormat[A](read: String => A)(write: A => String): Format[A] = Format[A](
    Reads[A](_.validate[String].map(read)),
    Writes[A](v => JsString(write(v)))
  )

  def uuidValueTypeFormat[A](read: String => A)(write: A => String): Format[A] = Format[A](
    Reads[A](_.validate[String].filter(JsError("Invalid UUID format"))(_.isUUID).map(read)),
    Writes[A](v => JsString(write(v)))
  )

  def intValueTypeFormat[A](read: Int => A)(write: A => Int): Format[A] = Format[A](
    Reads[A](_.validate[Int].map(read)),
    Writes[A](v => JsNumber(write(v)))
  )

  implicit lazy val codeJsonFormat: Format[Code] =
    stringValueTypeFormat[Code](Code)(_.value)

  implicit lazy val emailJsonFormat: Format[Email] =
    stringValueTypeFormat[Email](Email)(_.value)


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

  implicit lazy val subjectIdJsonFormat: Format[SubjectId] =
    uuidValueTypeFormat[SubjectId](SubjectId)(_.value)

  implicit lazy val signatureJsonFormat: Format[Signature] = Json.format[Signature]

  implicit lazy val traceJsonFormat: Format[Trace] = Json.format[Trace]

}

trait ExampleJsonFormat extends JsonFormat {

  implicit lazy val companyIdJsonFormat: Format[CompanyId] =
    uuidValueTypeFormat[CompanyId](CompanyId)(_.value)
  implicit lazy val companyDataJsonFormat: Format[CompanyData] = Json.format[CompanyData]
  implicit lazy val companyJsonFormat: Format[Company] = Json.format[Company]

  implicit lazy val employeeIdJsonFormat: Format[EmployeeId] =
    uuidValueTypeFormat[EmployeeId](EmployeeId)(_.value)
  implicit lazy val employeeRefJsonFormat: Format[EmployeeRef] = Json.format[EmployeeRef]
  implicit lazy val employeeDataJsonFormat: Format[EmployeeData] = Json.format[EmployeeData]
  implicit lazy val employeeJsonFormat: Format[Employee] = Json.format[Employee]



}
