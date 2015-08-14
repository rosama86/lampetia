package lampetia.security.sm

import lampetia.meta._
import lampetia.meta.feature._
import lampetia.model._
import play.api.libs.json.JsValue
import scala.util.{Success, Try}

case class SubjectId(value: String) extends AnyVal

sealed trait AccountState extends Any {
  def value: String
}

case object AccountActive extends AccountState {
  val value = "ACTIVE"
}

case object AccountSuspended extends AccountState {
  val value = "SUSPENDED"
}

object AccountState {
  def apply(value: String): AccountState = value match {
    case s if s == AccountActive.value => AccountActive
    case s if s == AccountSuspended.value => AccountSuspended
  }
}

case class UserId(value: String) extends AnyVal
case class User(id: UserId, accountState: AccountState) {
  def isActive = accountState == AccountActive
}


sealed trait AuthenticationProvider extends Any {
  def value: String
}
case object UsernamePasswordProvider extends AuthenticationProvider {
  val value = "USERNAME_PASSWORD"
}
case object Facebook extends AuthenticationProvider {
  val value = "FACEBOOK"
}
case object Twitter extends AuthenticationProvider {
  val value = "TWITTER"
}

object AuthenticationProvider {
  def apply(value: String): AuthenticationProvider = value match {
    case s if s == UsernamePasswordProvider.value => UsernamePasswordProvider
    case s if s == Facebook.value                 => Facebook
    case s if s == Twitter.value                  => Twitter
  }
}


case class ProviderUserId(value: String) extends AnyVal
case class ProviderResponse(json: JsValue) extends AnyVal
case class AccountDetails(json: JsValue) extends AnyVal
case class ProfileId(value: String) extends AnyVal
case class Password(value: String) extends AnyVal
case class ProfileRef(userId: UserId)
case class ProfileData(
  provider: AuthenticationProvider,
  providerUserId: ProviderUserId,
  providerResponse: ProviderResponse,
  email: Email,
  password: Option[Password],
  accountDetails: AccountDetails)
case class Profile(id: ProfileId, ref: ProfileRef, data: ProfileData)

sealed trait SubjectType {
  def value: String
}
case object SubjectUser extends SubjectType {
  val value = "USER"
}
case object SubjectGroup extends SubjectType {
  val value = "GROUP"
}
case object SubjectApplication extends SubjectType {
  val value = "APPLICATION"
}
case object SubjectSystem extends SubjectType {
  val value = "SYSTEM"
}

object SubjectType {
  def apply(value: String) = value match {
    case s if s == SubjectUser.value        => SubjectUser
    case s if s == SubjectGroup.value       => SubjectGroup
    case s if s == SubjectApplication.value => SubjectApplication
    case s if s == SubjectSystem.value      => SubjectSystem
  }

}

case class Subject(subjectId: SubjectId, subjectType: SubjectType)

case class GroupId(value: String) extends AnyVal
case class GroupRef(owner: UserId, parent: Option[GroupId])
case class GroupData(code: Code)
case class Group(id: GroupId, ref: GroupRef, data: GroupData)

case class GroupMemberRef(groupId: GroupId, memberId: SubjectId)
case class GroupMember(ref: GroupMemberRef)

case class Permission(code: Int) extends AnyVal {
  def add(other: Permission): Permission = copy(code = code | other.code)
  def |(other: Permission) = this add other
  def remove(other: Permission): Permission = copy(code = code & ~other.code)
  def exists(mask: Permission): Boolean = (code & mask.code) == mask.code
}


case class AclId(value: String) extends AnyVal
case class AclData(subject: Subject, resourceUri: ResourceUri, permission: Permission)
case class Acl(id: AclId, data: AclData)

