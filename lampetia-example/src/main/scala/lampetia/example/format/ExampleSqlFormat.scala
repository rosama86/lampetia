package lampetia.example.format

import lampetia.sql.SqlCodec
import org.joda.time.DateTime
import lampetia.example.model._


/**
  * @author Hossam Karim
  */
trait SqlFormat {

  val dialect: SqlCodec
  import dialect._


  implicit lazy val consumeName: Consume[Name] = consume[String].fmap(Name)
  implicit lazy val consumeNameOption: Consume[Option[Name]] = consume[Option[String]].fmap(_.map(Name))
  implicit lazy val produceName: Produce[Name] = a => produce(a.value)

  implicit lazy val consumeTitle: Consume[Title] = consume[String].fmap(Title)
  implicit lazy val consumeTitleOption: Consume[Option[Title]] = consume[Option[String]].fmap(_.map(Title))
  implicit lazy val produceTitle: Produce[Title] = a => produce(a.value)

  implicit lazy val consumeEmail: Consume[Email] = consume[String].fmap(Email)
  implicit lazy val consumeEmailOption: Consume[Option[Email]] = consume[Option[String]].fmap(_.map(Email))
  implicit lazy val produceEmail: Produce[Email] = a => produce(a.value)

  implicit lazy val consumeUrl: Consume[Url] = consume[String].fmap(Url)
  implicit lazy val consumeUrlOption: Consume[Option[Url]] = consume[Option[String]].fmap(_.map(Url))
  implicit lazy val produceUrl: Produce[Url] = a => produce(a.value)
  implicit lazy val produceUrlOption: Produce[Option[Url]] = a => produce(a.map(_.value))

  implicit lazy val consumeLocale: Consume[Locale] = consume[String].fmap(Locale)
  implicit lazy val consumeLocaleOption: Consume[Option[Locale]] = consume[Option[String]].fmap(_.map(Locale))
  implicit lazy val produceLocale: Produce[Locale] = a => produce(a.value)

  implicit lazy val consumePhone: Consume[Phone] = consume[String].fmap(Phone)
  implicit lazy val consumePhoneOption: Consume[Option[Phone]] = consume[Option[String]].fmap(_.map(Phone))
  implicit lazy val producePhone: Produce[Phone] = a => produce(a.value)

  implicit lazy val consumeCode: Consume[Code] = consume[String].fmap(Code)
  implicit lazy val produceCode: Produce[Code] = a => produce(a.value)

  implicit lazy val consumeSubjectId: Consume[SubjectId] = consume[String].fmap(SubjectId)
  implicit lazy val produceSubjectId: Produce[SubjectId] = a => produce(a.value)

  implicit lazy val consumeSignature: Consume[Signature] = (consume[SubjectId] and consume[DateTime])(Signature)
  implicit lazy val produceSignature: Produce[Signature] = a => produce(a.by) andThen produce(a.at)

  implicit lazy val consumeTrace: Consume[Trace] = (consume[Signature] and consume[Signature])(Trace)
  implicit lazy val produceTrace: Produce[Trace] = a => produce(a.created) andThen produce(a.updated)

}


trait ExampleSqlFormat extends SqlFormat {

  import dialect._

  implicit lazy val consumeCompanyId: Consume[CompanyId] = consume[String].fmap(CompanyId)
  implicit lazy val produceCompanyId: Produce[CompanyId] = a => produce(a.value)
  implicit lazy val consumeCompanyData: Consume[CompanyData] = consume[Name].fmap(CompanyData)
  implicit lazy val produceCompanyData: Produce[CompanyData] = a => produce(a.name)
  implicit lazy val consumeCompany: Consume[Company] = (consume[CompanyId] and consume[CompanyData])(Company)
  implicit lazy val produceCompany: Produce[Company] = a => produce(a.id) andThen produce(a.data)

  implicit lazy val consumeEmployeeId: Consume[EmployeeId] = consume[String].fmap(EmployeeId)
  implicit lazy val produceEmployeeId: Produce[EmployeeId] = a => produce(a.value)
  implicit lazy val consumeEmployeeRef: Consume[EmployeeRef] = consume[CompanyId].fmap(EmployeeRef)
  implicit lazy val produceEmployeeRef: Produce[EmployeeRef] = a => produce(a.company)
  implicit lazy val consumeEmployeeData: Consume[EmployeeData] = (consume[Name] and consume[Title])(EmployeeData)
  implicit lazy val produceEmployeeData: Produce[EmployeeData] = a => produce(a.name) andThen produce(a.title)
  implicit lazy val consumeEmployee: Consume[Employee] =
    (consume[EmployeeId] and consume[EmployeeRef] and consume[EmployeeData])(Employee)
  implicit lazy val produceEmployee: Produce[Employee] =
    a => produce(a.id) andThen produce(a.ref) andThen produce(a.data)


}
