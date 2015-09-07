package lampetia.mp

import org.scalatest.{Matchers, FlatSpec}

/**
 * @author Hossam Karim
 */

class MessagePackCodecSpec extends FlatSpec with Matchers {

  case class Simple(value: String)
  case class SimpleOption(value: Option[String])
  case class Address(street: String, city: Option[String], country: Option[String])
  case class Person(name: String, address: Address)
  case class PeopleGroup(name: String, people: Seq[Person])

  object MessagePackFormats extends MessagePackCodec {

    implicit lazy val consumeSimple: Consume[Simple] = consume[String].fmap(Simple)
    implicit lazy val produceSimple: Produce[Simple] = a => produce(a.value)

    implicit lazy val consumeSimpleOption: Consume[SimpleOption] = consume[Option[String]].fmap(SimpleOption)
    implicit lazy val produceSimpleOption: Produce[SimpleOption] = a => produce(a.value)

    implicit lazy val consumeAddress: Consume[Address] =
      (consume[String] and consume[Option[String]] and consume[Option[String]])(Address)
    implicit lazy val produceAddress: Produce[Address] =
     a => produce(a.street) andThen produce(a.city) andThen produce(a.country)

    implicit lazy val consumePerson: Consume[Person] =
      (consume[String] and consume[Address])(Person)
    implicit lazy val producePerson: Produce[Person] =
      a => produce(a.name) andThen produce(a.address)

    implicit lazy val consumePeopleGroup: Consume[PeopleGroup] =
      (consume[String] and consume[Seq[Person]])(PeopleGroup)
    implicit lazy val producePeopleGroup: Produce[PeopleGroup] =
    a => produce(a.name) andThen produce(a.people)
  }

  import MessagePackFormats._

  it should "apply message-pack roundtrip - Simple" in {

    val in = Simple("simple")

    val out = in.pack.unpack[Simple]

    in shouldEqual out

  }


  it should "apply message-pack roundtrip - SimpleOption" in {

    val in = SimpleOption(None)

    val out = in.pack.unpack[SimpleOption]

    in shouldEqual out

  }

  it should "apply message-pack roundtrip - Address" in {

    val in = Address("street", Some("city"), None)

    val out = in.pack.unpack[Address]

    in shouldEqual out

  }

  it should "apply message-pack roundtrip - Person" in {

    val in = Person("person", Address("street", Some("city"), None))

    val out = in.pack.unpack[Person]

    in shouldEqual out

  }

  it should "apply message-pack roundtrip - Seq[Simple]" in {

    val in = Seq(Simple("1"), Simple("2"))

    val out = in.pack.unpack[Seq[Simple]]

    in shouldEqual out

  }

  it should "apply message-pack roundtrip - Seq[Option[Simple]]" in {

    val in: Seq[Option[Simple]] = Seq(Some(Simple("1")), None, Some(Simple("2")))

    val out = in.pack.unpack[Seq[Option[Simple]]]

    in shouldEqual out

  }

  it should "apply message-pack roundtrip - Seq[Option[SimpleOption]]" in {

    val in: Seq[Option[SimpleOption]] = Seq(Some(SimpleOption(Some("1"))), None, Some(SimpleOption(None)))

    val out = in.pack.unpack[Seq[Option[SimpleOption]]]

    in shouldEqual out

  }


  it should "apply message-pack roundtrip - PeopleGroup" in {

    val in = PeopleGroup(
      "group",
      Seq(
          Person("person-1", Address("street", Some("city"), None))
        , Person("person-2", Address("street", Some("city"), None))
      )
    )

    val out = in.pack.unpack[PeopleGroup]

    in shouldEqual out

  }



}
