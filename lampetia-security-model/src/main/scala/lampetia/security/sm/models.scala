package lampetia.security.sm

import lampetia.model.ResourceUri

object ModuleUris {
  val PageUri = ResourceUri("/page/:id")
  val DocumentUri = PageUri / ResourceUri("document/:id")
  val ConferenceUri = ResourceUri("/conference/:id")
  val AgendaUri = ConferenceUri / ResourceUri("agenda/:id")
  val VideoUri = ConferenceUri / ResourceUri("video/:id")
  val PresentationUri = ConferenceUri / ResourceUri("presentation/:id")
}

case class PageId(value: String)
case class PageData(name: String)
case class Page(id: PageId, data: PageData) {
  def uri = s"/page/${id.value}"
}


case class ConferenceId(value: String)
case class ConferenceData(name: String)
case class Conference(id: ConferenceId, data: ConferenceData) {
  def uri = s"/conference/${id.value}"
}

case class AgendaId(value: String)
case class AgendaData(name: String)
case class AgendaRef(conferenceId: ConferenceId)
case class Agenda(id: AgendaId, ref: AgendaRef, data: AgendaData)

case class VideoId(value: String)
case class VideoRef(conferenceId: ConferenceId)
case class VideoData(name: String)
case class Video(id: VideoId, ref: VideoRef, data: VideoData) {
  def uri = s"/conference/${ref.conferenceId.value}/video/${id.value}"
}

case class PresentationId(value: String)
case class PresentationRef(conferenceId: ConferenceId)
case class PresentationData(name: String)
case class Presentation(id: PresentationId, ref: PresentationRef, data: PresentationData) {
  def uri = s"/conference/${ref.conferenceId.value}/presentation/${id.value}"
}

case class DocumentId(value: String)
case class DocumentRef(pageId: PageId)
case class DocumentData(name: String)
case class Document(id: DocumentId, ref: DocumentRef, data: DocumentData) {
  def uri = s"/page/${ref.pageId.value}/document/${id.value}"
}
