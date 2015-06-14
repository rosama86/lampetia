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
    with    CanBuild1[Person, PersonData]
    with    CanBuild2[Person, PersonId, PersonData]
    with    UUIDGenerator {
    val modelName = "person"
    def parse(stringId: String) = Success(PersonId(stringId))
    def generate: PersonId = PersonId(generateStringId)
    object data extends DataModel[PersonData] with Lens[Person, PersonData] {

      val firstName: LensProperty[PersonData, String] =
        property[String]("firstName")
          .getter[PersonData](_.firstName)
          .setter( (data, v) => data.copy(firstName = v))
          .set(sql.name("first_name"))
          .set(json.name("first-name"))

      val lastName: LensProperty[PersonData, String] =
        property[String]("lastName")
          .getter[PersonData](_.lastName)
          .setter( (data, v) => data.copy(lastName = v))
          .set(sql.name("last_name"))
          .set(sql.`type`("text"))
          .set(json.name("last-name"))

      def get(instance: Person): PersonData = instance.data
      def set(instance: Person, value: PersonData): Person = instance.copy(data = value)
      val properties = Seq(firstName, lastName)
    }

    override def features = Seq(
      sql.name("person_t"),
      sql.primaryKey("person_t_pk")(id),
      sql.schema("tmp")
    )

    def build(data: PersonData): BuildResult[Person] = BuildSuccess(Person(generate, data))
    def build(id: PersonId, data: PersonData): BuildResult[Person] = BuildSuccess(Person(id, data))

  }

  case class Coffee(name: String, country: String, rating: Int)
  implicit object CoffeeModel extends Model[Coffee] {
    val modelName: String = "Coffee"
    val name = property[String]("name")
    val country = property[String]("country")
    val rating = property[Int]("rating")
    override val properties = Seq(name, country, rating)
  }

}
