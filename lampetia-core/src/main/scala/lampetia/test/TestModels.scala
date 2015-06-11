package lampetia.test

import java.util.UUID

import lampetia.model._
import scala.util.Success

/**
 * @author Hossam Karim
 */

object TestModels {


  case class PersonId(value: String)
  case class PersonData(firstName: String, lastName: String)
  case class Person(id: PersonId, data: PersonData)

  implicit object PersonModel
    extends Model[Person]
    with    HasId[Person, PersonId]
    with    CanGenerate[PersonId]
    with    CanParse[PersonId]
    with    HasData[Person, PersonData]
    with    CanCombine2[Person, PersonId, PersonData]
    with    UUIDGenerator {
    val name = "person"
    def parse(stringId: String) = Success(PersonId(stringId))
    def generate: PersonId = PersonId(generateStringId)
    object data extends DataModel[PersonData] {

      val firstName =
        property[String]("firstName")
          .set(sql.name("first_name"))
          .set(json.name("first-name"))

      val lastName =
        property[String]("lastName")
          .set(sql.name("last_name"))
          .set(sql.`type`("jsonb"))
          .set(json.name("last-name"))

      val properties = Seq(firstName, lastName)
    }

    override def features = Seq(
      sql.name("person_t"),
      sql.schema("tmp")
    )

    def combine(id: PersonId, data: PersonData): Person = Person(id, data)

  }





}
