package lampetia.example.model

import lampetia.meta._
import lampetia.meta.feature._
import lampetia.model._
import lampetia.model.util._
import org.joda.time.DateTime
import play.api.libs.json.JsValue

case class Email(value: String) extends AnyVal
case class Code(value: String) extends AnyVal
case class Name(value: String) extends AnyVal
case class Title(value: String) extends AnyVal
case class Url(value: String) extends AnyVal
case class Locale(value:String) extends AnyVal
case class Phone(value: String) extends AnyVal

case class SubjectId(value: String) extends AnyVal
case class Signature(by: SubjectId, at: DateTime)
case class Trace(created: Signature, updated: Signature)

case class DateTimestamp(createdAt: DateTime, updatedAt: DateTime)
case class DateTimeInterval(from: DateTime, to: DateTime)

case class JsonData(value: JsValue) extends AnyVal

case class RefData[R, D](ref: R, data: D)

case class CompanyId(value: String) extends AnyVal
case class CompanyData(name: Name)
case class Company(id: CompanyId, data: CompanyData)

case class EmployeeId(value: String) extends AnyVal
case class EmployeeRef(company: CompanyId)
case class EmployeeData(name: Name, title: Title)
case class Employee(id: EmployeeId, ref: EmployeeRef, data: EmployeeData)