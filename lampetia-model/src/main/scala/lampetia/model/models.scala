package lampetia.model

import java.util.UUID

import play.api.libs.json.{Json, JsValue}

trait IdGenerator {

  def generateStringId: String

}

trait UUIDGenerator extends IdGenerator {
  def generateStringId: String = UUID.randomUUID.toString
}

/*trait JSON extends Any {
  def stringify: String
}

case class PlayJson(json: JsValue) extends AnyVal with JSON {
  def stringify: String = Json.stringify(json)
}

trait JSONParser[T] {
  def parse(json: String): JSON
}

object JSON {
  def parse[T](json: String)(implicit parser: JSONParser[T]): JSON = parser.parse(json)
}*/





case class ResourceId(value: String) extends AnyVal
case class ResourceType(value: String) extends AnyVal
case class Resource(resourceId: ResourceId, resourceType: ResourceType)

case class Email(value: String) extends AnyVal
case class Code(value: String) extends AnyVal