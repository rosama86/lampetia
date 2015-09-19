package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.cg.extensions.Models
import lampetia.metamodel.Dsl._
import lampetia.metamodel._

/**
 * @author Radwa Osama
 * @since 1:21 PM - 9/4/2015
 */
object NextEventModule extends App {

  val base = "nxt.event"

  val module = Module("nxt-event", base, "nxt-event", organization = Some("nxt"), options = Seq(Secure), mn = Some("Event"))

  val SponsorshipTypeModel: Value = value("SponsorshipType")("value".string) <+ commonFeatures

  val EventStateModel =
    enum("EventState")(
      enumCase("EventLive", "LIVE"),
      enumCase("EventDraft", "DRAFT"),
      enumCase("EventCanceled", "CANCELED"),
      enumCase("EventArchived", "ARCHIVED"))
      .withDiscriminator("value".string) <+ commonFeatures

  val TrackTypeModel =
    enum("TrackType")(
      enumCase("SingleTrack", "SingleTrack"),
      enumCase("MultipleTrack", "MultipleTrack"))
      .withDiscriminator("value".string) <+ commonFeatures

  val UrlModel = value("Url")("value".string) <+ externalModelFeatures

  val NameModel = value("Name")("value".string) <+ commonFeatures

  val TitleModel = value("Title")("value".string) <+ commonFeatures

  val GroupModel = entity("Group")() <+ commonFeatures

  val LocaleModel = value("Locale")("value".string) <+ commonFeatures

  val LinkModel: Composite = composite("Link")("site".string, "link" of UrlModel) <+ commonFeatures

  val PhoneModel: Value = value("Phone")("value".string) <+ commonFeatures

  val OrganizerModel: Entity =
    entity("Organizer")(
      "name" of NameModel,
      "description" optionOf StringLiteral,
      "socialLinks" listOf LinkModel,
      "primaryUrl" optionOf UrlModel,
      "externalLinks" listOf LinkModel,
      "phones" listOf PhoneModel,
      "photoUrl" optionOf UrlModel)

  val TrackModel: Value = value("Track")("value".string) <+ commonFeatures

  val SessionLocationModel: Value = value("SessionLocation")("value".string) <+ commonFeatures

  val TracksModel: Composite = composite("Tracks")("value" listOf TrackModel) <+ commonFeatures

  val SponsorModel: Entity =
    entity("Sponsor")("name" of NameModel,
      "description" optionOf StringLiteral,
      "socialLinks" listOf LinkModel,
      "primaryUrl" optionOf UrlModel,
      "externalLinks" listOf LinkModel,
      "phones" listOf PhoneModel,
      "photoUrl" optionOf UrlModel,
      "sponsorshipType" optionOf SponsorshipTypeModel)

  val PartialEventModel: Entity = entity("Event")()

  val EventTicketModel: Entity =
    entity("EventTicket")(
      "ticketType".string, "availableQuantity".int, "price".double,
      "currency".string, "description" optionOf StringLiteral,
      "channelUrl" optionOf UrlModel,
      "eventId" ref PartialEventModel.id) <+ commonFeatures

  val UserModel: Entity = entity("User")()

  val EventModel: Entity =
    extend(PartialEventModel)(
      "title" of TitleModel,
      "about".string,
      "description" optionOf StringLiteral,
      "locationLatitude" optionOf StringLiteral,
      "locationLongitude" optionOf StringLiteral,
      "locationCity".string,
      "locationAddress".string,
      "locationName".string,
      "locationUrl" optionOf UrlModel,
      "locationPhotoUrl" optionOf UrlModel,
      "startDateTime".dateTime,
      "endDateTime".dateTime,
      "bannerUrl" optionOf UrlModel,
      "timezone" of LocaleModel,
      "eventState" of EventStateModel,
      ("socialLinks" listOf LinkModel) << (jsonbComposite in Sql),
      ("externalLinks" listOf LinkModel) << (jsonbComposite in Sql),
      ("phones" listOf PhoneModel) << (jsonbComposite in Sql),
      ("ticketInformation" listOf EventTicketModel) << (jsonbComposite in Sql),
      ("organizers" listOf OrganizerModel) << (jsonbComposite in Sql),
      ("sponsors" listOf SponsorModel) << (jsonbComposite in Sql),
      ("sponsorshipTypes" listOf SponsorshipTypeModel) << (jsonbComposite in Sql),
      "groupId" ref GroupModel.id
    )

  val SpeakerModel: Entity =
    entity("Speaker")(
      "name" of NameModel,
      "title" optionOf StringLiteral,
      "about" optionOf StringLiteral,
      "company" optionOf StringLiteral,
      ("socialLinks" listOf LinkModel) << (jsonbComposite in Sql),
      ("externalLinks" listOf LinkModel) << (jsonbComposite in Sql),
      "photoUrl" optionOf UrlModel,
      "eventId" ref EventModel.id)

  val SessionModel: Entity =
    entity("Session")(
      "name" of NameModel,
      "description" optionOf StringLiteral,
      "startDateTime".dateTime,
      "endDateTime".dateTime,
      "location" optionOf SessionLocationModel,
      "track" optionOf TracksModel,
      "eventId" ref EventModel.id)

  val SessionSpeakerModel: Entity =
    entity("SessionSpeaker")(
      "sessionId" ref SessionModel.id,
      "speakerId" ref SpeakerModel.id)

  val AgendaRowModel: Entity =
    entity("AgendaRow")(
      "time".dateTime, "trackType" of TrackTypeModel, "agendaCells" listOf SessionModel)

  val AgendaDayModel: Entity =
    entity("AgendaDay")(
      "day".dateTime, "tracks" of TracksModel, "agendaRows" listOf AgendaRowModel)

  val AgendaModel: Entity =
    entity("Agenda")("agendaDays" listOf AgendaDayModel, "eventId" ref EventModel.id)

  val models = Seq(
    LinkModel,
    SponsorshipTypeModel,
    EventStateModel,
    EventModel.id.tpe,
    EventModel,
    EventTicketModel.id.tpe,
    EventTicketModel,
    OrganizerModel.id.tpe,
    OrganizerModel,
    SponsorModel.id.tpe,
    SponsorModel,
    SpeakerModel.id.tpe,
    SpeakerModel,
    SessionModel.id.tpe,
    SessionModel,
    SessionSpeakerModel.id.tpe,
    SessionSpeakerModel,
    TrackTypeModel,
    TrackModel,
    SessionLocationModel,
    TracksModel,
    AgendaRowModel.id.tpe,
    AgendaRowModel,
    AgendaDayModel.id.tpe,
    AgendaDayModel,
    AgendaModel.id.tpe,
    AgendaModel)

  CodeGenerator.serviceGenerator(module, models).generate()
}
