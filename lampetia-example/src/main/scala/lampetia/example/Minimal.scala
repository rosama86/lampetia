package lampetia.example

import lampetia.sql.ConnectionSourceFactories

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}


// sample model
case class Person(id: String, name: String)

object Minimal {

  // import the SQL dialect
  import lampetia.sql.dialect.postgresql.Postgresql._

  // expose an implicit execution context
  import scala.concurrent.ExecutionContext.Implicits.global

  // create a connection source
  implicit lazy val connectionSource =
    ConnectionSourceFactories.hikariFromDataSourceClass(
      dataSourceClassName = "org.postgresql.ds.PGSimpleDataSource",
      serverName = "localhost",
      portNumber = 5432,
      databaseName = "db",
      user = "admin",
      password = "admin")

  // tell lampetia how to map a row into a Person model
  implicit lazy val consumePerson: Consume[Person] = (consume[String] and consume[String])(Person)

  // create table action
  val createTable = "create table person(id text, name text)".sql.write

  // drop table action
  val dropTable = "drop table person".sql.write

  // insert person action, notice the return type
  def insertPerson(p: Person): IO[Int] = sql"insert into person values (${p.id}, ${p.name})".write

  def main(args: Array[String]): Unit = {

    // a query parameter
    val person = Person("p1", "someone")

    // select person query, notice the return type
    val select: IO[Option[Person]] =
      sql"select id, name from person where id=${person.id} limit 1".read[Person].map(_.headOption)

    // create, insert, select, drop operations all in a single transaction (Postgres supports transactional DDL)
    val query = for {
      _ <- createTable.transactionally
      _ <- insertPerson(person)
      p <- select
      _ <- dropTable
    } yield p

    // so far, nothing has been issued to the database, we just created a series of actions
    // let's run those actions and get a future of the result
    val result: Future[Option[Person]] = query.run

    result.onComplete {
      case Success(Some(value)) => println(value)
      case Success(None)        => println("not found")
      case Failure(e)           => println(e.getMessage)
    }

    Await.ready(result, Duration.Inf)

    connectionSource.shutdown()
  }

}
