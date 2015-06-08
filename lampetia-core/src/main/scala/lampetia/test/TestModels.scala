package lampetia.test

import java.util.UUID

import lampetia.model._
import shapeless._
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
    with    HasData[Person, PersonData] {
    type Shape = PersonId :: PersonData :: HNil
    val name = "person"

    def id = property[PersonId]("id", _.id, e => v => e.copy(id = v))
    def parse(stringId: String) = Success(PersonId(stringId))
    def generate = PersonId(UUID.randomUUID.toString)

    object data extends DataModel[Person, PersonData] {

      val firstName: Property[PersonData, String] =
        property[String]("firstName", _.firstName, e => v => e.copy(firstName = v))
          .set(SqlFeature.name("first_name"))
          .set(JsonFeature.name("first-name"))

      val lastName: Property[PersonData, String] =
        property[String]("lastName", _.lastName, e => v => e.copy(lastName = v))
          .set(SqlFeature.name("last_name"))
          .set(SqlFeature.`type`("jsonb"))
          .set(JsonFeature.name("last-name"))

      val properties = Seq(firstName, lastName)
      def get(instance: Person): PersonData = instance.data
      def set(instance: Person, value: PersonData): Person = instance.copy(data = value)

    }

    override def features = Seq(
      SqlFeature.name("person_t"),
      SqlFeature.schema("tmp")
    )

    def combine(hl: PersonId::PersonData::HNil): Person = hl match {
      case id::data::HNil => Person(id, data)
    }

  }



}
