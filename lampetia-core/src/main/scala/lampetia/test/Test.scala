package lampetia.test

import lampetia.model._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * @author Hossam Karim
 */

object Test extends App {
  import lampetia.sql.dsl.dialect.postgres._
  import scala.concurrent.ExecutionContext.Implicits.global

  case class Person(firstName: String, lastName: String)

  object PersonModel extends Model[Person] with HasData[Person] {
    type Data = Person
    val name = "person"
    object data extends DataModel[Person] {

      val firstName =
        property[String]("firstName")
          .set(JsonFeature.name("uuid"))

      val lastName =
        property[String]("lastName")
          .set(SqlFeature.name("last_name"))
          .set(JsonFeature.name("last-name"))

      def properties: Seq[Property[_]] = Seq(firstName, lastName)
    }

    override def features = Seq(
      SqlFeature.name("person-table")
    )
  }

  println(PersonModel.sqlName)

  PersonModel.properties.foreach(p => println(s"${p.sqlName} => ${p.sqlType}"))
  PersonModel.properties.foreach(p => println(p.jsonName))


  /*implicit val context: ConnectionSource =
    hikari(
      "org.postgresql.ds.PGSimpleDataSource",
      "localhost", 5432, "jeelona", "admin", "admin", 3, 2000)

  val f =
    select(1.literal)
    .lifted
    .readSqlIO[Int]
    .run
  f.onSuccess { case v => println(v) }
  f.onFailure { case e => println(e) }

  Await.ready(f, Duration.Inf)
  context.shutdown()*/

}
