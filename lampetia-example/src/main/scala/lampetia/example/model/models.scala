package lampetia.example.model

import lampetia.meta._
import lampetia.meta.feature._
import lampetia.model._
import lampetia.model.util._
import org.joda.time.DateTime
import play.api.libs.json.JsValue

import scala.util.Try

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

trait ExampleModel {

  implicit object CompanyModel
    extends Model[Company]
    with HasId[Company, CompanyId]
    with HasData[Company, CompanyData]
    with CanGenerate[CompanyId]
    with CanParse[CompanyId]
    with UUIDGenerator {

    val modelName = "Company"
    val id = property[CompanyId]("id")

    def generate = CompanyId(generateStringId)
    def parse(stringId: String): Try[CompanyId] = parseUUID(stringId)(CompanyId)

    object data extends DataModel[CompanyData] {
      val name = property[Name]("name")
      val properties = Seq(name)
    }

    override val features: Seq[Feature] = Seq(
      sql.primaryKey("company_pk")(id)
    )

  }

  implicit object EmployeeModel
    extends Model[Employee]
    with HasId[Employee, EmployeeId]
    with HasData[Employee, EmployeeData]
    with HasRef[Employee, EmployeeRef]
    with CanGenerate[EmployeeId]
    with CanParse[EmployeeId]
    with UUIDGenerator {

    val modelName = "Employee"
    val id = property[EmployeeId]("id")

    def generate = EmployeeId(generateStringId)
    def parse(stringId: String): Try[EmployeeId] = parseUUID(stringId)(EmployeeId)

    object data extends DataModel[EmployeeData] {
      val name = property[Name]("name")
      val title = property[Title]("title")
      val properties = Seq(name, title)
    }

    object ref extends RefModel[EmployeeRef] {
      val company = property[CompanyId]("company")
      val properties = Seq(company)
    }

    override val features: Seq[Feature] = Seq(
      sql.primaryKey("employee_pk")(id),
      sql.foreignKey("employee_ref_company_id")(ref.company)(CompanyModel, CompanyModel.id)
    )

  }
}