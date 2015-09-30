package lampetia.model

import java.util.UUID

trait IdGenerator {
  def generateStringId: String
}

trait UUIDGenerator extends IdGenerator {
  def generateStringId: String = UUID.randomUUID.toString
}

