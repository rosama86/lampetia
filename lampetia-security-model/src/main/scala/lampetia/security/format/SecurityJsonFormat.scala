package lampetia.security.format

import lampetia.format.JsonFormat
import lampetia.security.model._
import play.api.libs.json._

/**
 * @author Hossam Karim
 */

trait SecurityJsonFormat extends JsonFormat {

  implicit lazy val subjectIdJsonFormat: Format[SubjectId] =
    valueTypeFormat[SubjectId](SubjectId)(_.value)

  implicit lazy val subjectTypeJsonFormat: Format[SubjectType] =
    valueTypeFormat[SubjectType](SubjectType.apply)(_.value)

  implicit lazy val subjectJsonFormat: Format[Subject] = Json.format[Subject]

  implicit lazy val accountStateJsonFormat: Format[AccountState] =
    valueTypeFormat[AccountState](AccountState.apply)(_.value)

  implicit lazy val userIdJsonFormat: Format[UserId] =
    valueTypeFormat[UserId](UserId)(_.value)

  implicit lazy val userJsonFormat: Format[User] = Json.format[User]

  implicit lazy val authenticationProviderJsonFormat: Format[AuthenticationProvider] =
    valueTypeFormat[AuthenticationProvider](AuthenticationProvider.apply)(_.value)

  implicit lazy val profileIdJsonFormat: Format[ProfileId] =
    valueTypeFormat[ProfileId](ProfileId)(_.value)

  implicit lazy val passwordJsonFormat: Format[Password] =
    valueTypeFormat[Password](Password)(_.value)

  implicit lazy val providerResponseJsonFormat: Format[ProviderResponse] = Json.format[ProviderResponse]

  implicit lazy val accountDetailsJsonFormat: Format[AccountDetails] = Json.format[AccountDetails]

  implicit lazy val profileRefJsonFormat: Format[ProfileRef] = Json.format[ProfileRef]

  implicit lazy val providerUserIdFormat: Format[ProviderUserId] =
    valueTypeFormat[ProviderUserId](ProviderUserId)(_.value)

  implicit lazy val profileDataJsonFormat: Format[ProfileData] = Json.format[ProfileData]

  implicit lazy val profileJsonFormat: Format[Profile] = Json.format[Profile]

  implicit lazy val permissionJsonFormat: Format[Permission] = Format[Permission] (
    Reads[Permission](_.validate[Int].map(Permission)),
    Writes[Permission](v => JsNumber(v.code))
  )

  implicit lazy val groupIdJsonFormat: Format[GroupId] =
    valueTypeFormat[GroupId](GroupId)(_.value)

  implicit lazy val groupRefJsonFormat: Format[GroupRef] = Json.format[GroupRef]

  implicit lazy val groupDataJsonFormat: Format[GroupData] = Json.format[GroupData]

  implicit lazy val groupJsonFormat: Format[Group] = Json.format[Group]




}
