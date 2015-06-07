package lampetia.test

import lampetia.model.Model

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * @author Hossam Karim
 */

object Test extends App {

  case class Person(firstName: String, lastName: String)

  object PersonModel extends Model[Person] {
    val sqlName = "person"
    val firstName = property[String]("first_name")
    val lastName = property[String]("last_name")
    override val properties = Seq(firstName, lastName)
  }

  import lampetia.sql.dsl.dialect.postgres._
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val context: ConnectionSource =
    hikari(
      "org.postgresql.ds.PGSimpleDataSource",
      "localhost", 5432, "jeelona", "admin", "admin", 3, 2000)

  val f =
    select(1.literal)
    .from(PersonModel)
    .lifted
    .readSqlIO[Int]
    .run
  f.onSuccess { case v => println(v) }
  f.onFailure { case e => println(e) }

  Await.ready(f, Duration.Inf)
  context.shutdown()

}
