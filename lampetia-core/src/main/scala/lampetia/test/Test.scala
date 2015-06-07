package lampetia.test

import lampetia.model.Model

/**
 * @author Hossam Karim
 */

object Test {

  case class Person(firstName: String, lastName: String)

  object PersonModel extends Model[Person] {
    val sqlName = "person"
    val firstName = property[String]("first_name")
    val lastName = property[String]("last_name")
    override val properties = Seq(firstName, lastName)
  }

  import lampetia.sql.dsl.dialect.postgres._
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val context: ConnectionSource = ???

  val q =
    select(1.literal)
    .from(PersonModel)
    .lifted
    .readSqlIO[Int]
    .run

}
