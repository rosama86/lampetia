package lampetia.model

import java.util.UUID

import play.api.libs.json.{Json, JsValue}

trait IdGenerator {

  def generateStringId: String

}

trait UUIDGenerator extends IdGenerator {
  def generateStringId: String = UUID.randomUUID.toString
}

case class ResourceId(value: String) extends AnyVal
case class ResourceUri(value: String) extends AnyVal {
  def /(child: ResourceUri): ResourceUri = ResourceUri(s"$value/${child.value}")
  def * = ResourceUri(s"$value/.*")
}
case class Resource(resourceId: ResourceId, resourceUri: ResourceUri)

case class UriPattern(value: String) extends AnyVal {
  def * = UriPattern(s"$value/.*")
}

case class Email(value: String) extends AnyVal
case class Code(value: String) extends AnyVal