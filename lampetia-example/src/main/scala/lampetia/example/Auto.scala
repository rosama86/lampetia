package lampetia.example

import lampetia.example.model._
import lampetia.example.module.ExampleModule

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}


object Auto {

  import ExampleModule.dialect._
  import ExampleModule.sql._

  implicit val connectionSource = ExampleModule.connectionSource
  implicit val executionContext = ExampleModule.executionContext


  def main(args: Array[String]): Unit = {

    val model = CompanyModel

    val actions = for {

      _ <- model.create.transactionally

      _ <- model.insert(model.id := CompanyId("c1").bind, model.data.name := Name("some company").bind)

      v <- model.findOne(model.id === CompanyId("c1").bind)

      _ <- model.update(model.data.name := Name("something").bind)(model.id === CompanyId("c1").bind)

      _ <- model.delete(model.data.name like "%s".literal)

      _ <- model.drop(cascade = false)

    } yield v

    val result = actions.run

    result.onComplete {
      case Success(Some(company)) => println(company)
      case Success(None)          => println("not found")
      case Failure(e)             => println(e.getMessage)
    }

    Await.ready(result, Duration.Inf)

    ExampleModule.terminate()



  }

}
