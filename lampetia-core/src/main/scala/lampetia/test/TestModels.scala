package lampetia.test

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
    with    CanCombine1[Person, PersonData]
    with    CanCombine2[Person, PersonId, PersonData]
    with    UUIDGenerator {
    val name = "person"
    def parse(stringId: String) = Success(PersonId(stringId))
    def generate: PersonId = PersonId(generateStringId)
    object data extends DataModel[PersonData] {

      val firstName: LensProperty[Person, String] =
        property[String]("firstName")
          .getter[Person](_.data.firstName)
          .setter( (person, v) => person.copy(data = person.data.copy(firstName = v)))
          .set(sql.name("first_name"))
          .set(json.name("first-name"))

      val lastName: LensProperty[Person, String] =
        property[String]("lastName")
          .getter[Person](_.data.lastName)
          .setter( (person, v) => person.copy(data = person.data.copy(lastName = v)))
          .set(sql.name("last_name"))
          .set(sql.`type`("jsonb"))
          .set(json.name("last-name"))

      val properties = Seq(firstName, lastName)
    }

    override def features = Seq(
      sql.name("person_t"),
      sql.schema("tmp")
    )

    def combine(data: PersonData): Person = Person(generate, data)
    def combine(id: PersonId, data: PersonData): Person = Person(id, data)

  }





}
