package lampetia.example.service

import lampetia.example.model._

/**
  * @author Hossam Karim
  */

trait CompanyService {

  import lampetia.example.module.ExampleModule.dialect._
  import lampetia.example.module.ExampleModule.sql._

  def findOne(id: CompanyId): IO[Option[Company]] =
    sql"select * from company where id = $id".read[Company].map(_.headOption)

}
