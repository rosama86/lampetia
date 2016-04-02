package lampetia.example.service

import lampetia.example.model._
import lampetia.sql.ast.Operator

/**
  * @author Hossam Karim
  */

trait CompanyService {

  import lampetia.example.module.ExampleModule.dialect._
  import lampetia.example.module.ExampleModule.sql._

  def insert(data: CompanyData): IO[Company] = {
    val m = CompanyModel
    val id = m.generate
    m.insert(m.id := id.bind, m.data.name := data.name.bind).map(_ => Company(id, data))
  }

  def findOne(id: CompanyId): IO[Option[Company]] =
    sql"select id, name from company where id = $id".read[Company].map(_.headOption)

  def find: IO[Seq[Company]] =
    select(CompanyModel.properties:_*).from(CompanyModel).lifted.read[Company]

  def find[F <: Operator](filter: F): IO[Seq[Company]] = {
    val m = CompanyModel
    select(m.properties:_*).from(m).where(filter).lifted.read[Company]
  }

  def findEmployees(id: CompanyId): IO[Seq[Employee]] = {
    val c = CompanyModel
    val e = EmployeeModel

    select(e.properties:_*)
    .from(c.innerJoin(e).on(e.ref.company === c.id))
    .where(c.id === id.bind)
    .lifted
    .read[Employee]
  }

}
