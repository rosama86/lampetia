package lampetia.model

import java.util.UUID

import org.joda.time.DateTime

trait IdGenerator {

  def generateStringId: String

}

trait UUIDGenerator extends IdGenerator {
  override def generateStringId: String = UUID.randomUUID.toString
}

case class ResourceId(value: String) extends AnyVal
case class ResourceUri(value: String) extends AnyVal {
  def /(child: ResourceUri): ResourceUri = ResourceUri(s"$value/${child.value}")
  def childResourcesUri: ResourceUri = ResourceUri(s"$value/*")
  def pattern: UriPattern = UriPattern(value)
}
case class Resource(resourceId: ResourceId, resourceUri: ResourceUri)

case class UriPattern(value: String) extends AnyVal {
  def * = UriPattern(s"$value/.*")
}

case class UserId(value: String) extends AnyVal
case class Email(value: String) extends AnyVal
case class Code(value: String) extends AnyVal
case class Name(value: String) extends AnyVal
case class Title(value: String) extends AnyVal
case class Url(value: String) extends AnyVal
case class Locale(value:String) extends AnyVal
case class Phone(value: String) extends AnyVal

case class Signature(by: UserId, at: DateTime)
case class Trace(created: Signature, updated: Signature)