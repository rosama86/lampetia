package lampetia.example

import lampetia.example.model._
import lampetia.example.module.ExampleModule

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}


object Example {

  implicit val connectionSource = ExampleModule.connectionSource
  implicit val executionContext = ExampleModule.executionContext

  import ExampleModule.dialect._
  import ExampleModule.sql._



  def main(args: Array[String]): Unit = {
    val company = CompanyModel
    val employee = EmployeeModel

    val actions = for {

      _ <- company.create.transactionally

      _ <- employee.create

      _ <- company.insert(
        company.id := CompanyId("c1").bind,
        company.data.name := Name("c1").bind)

      _ <- employee.insert(
        employee.id := EmployeeId("e1").bind,
        employee.ref.company := CompanyId("c1").bind,
        employee.data.name := Name("e1").bind,
        employee.data.title := Title("e1").bind)

      es <- select(employee.properties.map('e dot _):_*)
        .from(company as 'c innerJoin employee as 'e on ('c dot company.id === 'e dot employee.ref.company))
        .where('c dot company.id === CompanyId("c1").bind)
        .limit(1.literal)
        .lifted
        .read[Employee]


      _ <- employee.drop(cascade = true)

      _ <- company.drop(cascade = true)


    } yield es

    val result = actions.run

    result.onComplete {
      case Success(x) => println(x)
      case Failure(e) => println(e.getMessage)
    }


    Await.ready(result, Duration.Inf)

    ExampleModule.configuration.shutdown()
  }

}
