package lampetia.security.service

import java.util.UUID

import lampetia.model.{UserId, Email}
import lampetia.security.model._
import play.api.libs.json.Json

/**
 * @author Radwa Osama
 */
trait CommonServiceSpec {

  final val EMPTY = ""

  def groupRef(ownerId: UserId, parentGroupId: Option[GroupId] = None): GroupRef = GroupRef(ownerId, parentGroupId)

  def testProfileData = {
    val email = s"${UUID.randomUUID.toString}@test.org"
    ProfileData(
      UsernamePasswordProvider,
      ProviderUserId(""),
      ProviderResponse(Json.parse("[]")),
      Email(email),
      Some(Password("unsafe")),
      AccountDetails(Json.parse("[]")))
  }
}
