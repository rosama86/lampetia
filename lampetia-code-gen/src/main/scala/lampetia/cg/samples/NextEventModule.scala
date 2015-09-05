package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.{Entity, Module}

/**
 * @author Radwa Osama
 * @since 1:21 PM - 9/4/2015
 */
object NextEventModule extends App {

  val base = "nxt.event"

  val module = Module("nxt-event", base, "nxt-event", mn = Some("Event"))

  val EventStateModel =
    enum("EventState")(
      enumCase("EventLive", "LIVE"), enumCase("EventDraft", "DRAFT"), enumCase("EventPast", "PAST"))
      .withDiscriminator("value".string) <+ commonFeatures

  val EventPrivacyModel =
    enum("EventPrivacy")(
      enumCase("EventPublic", "PUBLIC"), enumCase("EventPrivate", "PRIVATE"))
      .withDiscriminator("value".string) <+ commonFeatures

  val employeePartialModel = entity("Employee")()

  val departmentModel: Entity =
    entity("Department")(
      "name".name,
      "manager" ref employeePartialModel.id)

  val agreement =
    composite("Agreement")("percentage".int, "vesting".string)

  val employeeStockOptions =
    composite("StockOptions")(
      ("agreement" of agreement) << (flatten() in Sql),
      "bank".name,
      "degree".int,
      "since".dateTime)

  val employeeModel: Entity =
    extend(employeePartialModel)(
      "name".name,
      "salary".int,
      "department" optionalRef departmentModel.id,
      ("options" of employeeStockOptions) << (flatten() in Sql))

  val organizerModel: Entity =
    entity("Organizer")("name".string, "description".string, "contactInfo".json, "photoUrl".string)

  val speakerModel: Entity =
    entity("Speaker")("name".string, "description".string, "contactInfo".json, "photoUrl".string)

  val sessionModel: Entity =
    entity("Session")("name".string, "description".string, "contactInfo".json, "photoUrl".string)

  val eventModel: Entity =
    entity("Event")(
      "title".string,
      "about".string,
      "description".string,
      "agenda".string,
      "sessions".string,
      "location".string,
      "address".string,
      "place".string,
      "startDate".dateTime,
      "endDate".dateTime,
      "bannerUrl".string,
      "timezone".string,
      "eventState" of EventStateModel,
      "eventPrivacy" of EventPrivacyModel,
      "url".string,
      "ticketInformation".jsvalue,
      "organizerId" ref organizerModel.id
    )

  val eventAttendeeModel: Entity =
    entity("EventAttendee")("eventId" ref eventModel.id /*, "attendeeId" ref UserId*/)

  val models = Seq(
    EventStateModel,
    EventPrivacyModel,
    eventModel.id.tpe,
    eventModel,
    eventAttendeeModel,
    organizerModel.id.tpe,
    organizerModel,
    speakerModel.id.tpe,
    speakerModel,
    sessionModel.id.tpe,
    sessionModel)

  CodeGenerator.modelGenerator(module, models).generate()
}
