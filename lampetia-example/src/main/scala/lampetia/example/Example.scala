package lampetia.example

import lampetia.example.model.{Company, Employee}
import lampetia.example.module.ExampleModule

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * @author Hossam Karim
  */


object Example {

  implicit val connectionSource = ExampleModule.connectionSource
  import ExampleModule.dialect._
  import ExampleModule.sql._

  import scala.concurrent.ExecutionContext.Implicits.global


  def main(args: Array[String]): Unit = {
    val query =
      for {
        // postgres supports transactional DDL, start a transaction
        _ <- "create table company(id text, name text)".sql.write.transactionally
        // plain sql
        _ <- "create table employee(id text, company text, name text, title text)".sql.write
        // un-named parameters
        _ <- "insert into company values(?, ?)".sql.set("c1").set("c1").write
        // sql interpreted strings
        _ <- sql"insert into company values(${"c2"}, ${"c2"})".write
        // named parameters
        _ <- "insert into employee values(#{id}, #{company}, #{name}, #{name})"
              .sql.set("id", "e1").set("company", "c1").set("name", "e1").write
        // implicit mapping
        c <- "select * from company".sql.read[Company]
        e <- "select * from employee".sql.read[Employee].map(_.headOption)
        // drop those tables
        _ <- "drop table employee".sql.write
        _ <- "drop table company".sql.write
      } yield c

    val result = query.run

    result.onSuccess {
      case c => println(c)
    }

    result.onFailure {
      case e => println(e.getMessage)
    }

    Await.ready(result, Duration.Inf)

    ExampleModule.configuration.shutdown()
  }

}
