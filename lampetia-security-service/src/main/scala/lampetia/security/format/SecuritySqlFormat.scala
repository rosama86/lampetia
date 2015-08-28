package lampetia.security.format

import lampetia.codec.{OptionCodecs, PrimitiveCodecs, Codec}
import lampetia.model._
import lampetia.security.model._
import play.api.libs.json.Json

/**
 * @author Hossam Karim
 */

trait SecuritySqlFormat { self: Codec with PrimitiveCodecs with OptionCodecs =>

  implicit lazy val consumeSubjectId: Consume[SubjectId] = consume[String].fmap(SubjectId)
  implicit lazy val produceSubjectId: Produce[SubjectId] = a => produce(a.value)

  implicit lazy val consumeSubjectType: Consume[SubjectType] = consume[String].fmap(SubjectType.apply)
  implicit lazy val produceSubjectType: Produce[SubjectType] = a => produce(a.value)

  implicit lazy val consumeSubject: Consume[Subject] = (consume[SubjectId] ~ consume[SubjectType])(Subject)
  implicit lazy val produceSubject: Produce[Subject] = a => produce(a.subjectId) andThen produce(a.subjectType)

  implicit lazy val consumeAccountState: Consume[AccountState] = consume[String].fmap(AccountState.apply)
  implicit lazy val produceAccountState: Produce[AccountState] = a => produce(a.value)

  implicit lazy val consumeUserId: Consume[UserId] = consume[String].fmap(UserId)
  implicit lazy val produceUserId: Produce[UserId] = a => produce(a.value)

  implicit lazy val consumeUser: Consume[User] = (consume[UserId] and consume[AccountState])(User)
  implicit lazy val produceUser: Produce[User] = a => produce(a.id) andThen produce(a.accountState)

  implicit lazy val consumeAuthenticationProvider: Consume[AuthenticationProvider] = consume[String].fmap(AuthenticationProvider.apply)
  implicit lazy val produceAuthenticationProvider: Produce[AuthenticationProvider] = a => produce(a.value)

  implicit lazy val consumeProfileId: Consume[ProfileId] = consume[String].fmap(ProfileId)
  implicit lazy val produceProfileId: Produce[ProfileId] = a => produce(a.value)

  implicit lazy val consumeProviderUserId: Consume[ProviderUserId] = consume[String].fmap(ProviderUserId)
  implicit lazy val produceProviderUserId: Produce[ProviderUserId] = a => produce(a.value)

  implicit lazy val consumeEmail: Consume[Email] = consume[String].fmap(Email)
  implicit lazy val produceEmail: Produce[Email] = a => produce(a.value)

  implicit lazy val consumePassword: Consume[Password] = consume[String].fmap(Password)
  implicit lazy val consumePasswordOption: Consume[Option[Password]] = consume[Option[String]].fmap(_.map(Password))
  implicit lazy val producePassword: Produce[Password] = a => produce(a.value)
  implicit lazy val producePasswordOption: Produce[Option[Password]] = a => produce(a.map(_.value))

  implicit lazy val consumeProviderResponse: Consume[ProviderResponse] =
    consume[String].fmap(Json.parse).fmap(ProviderResponse)
  implicit lazy val produceProviderResponse: Produce[ProviderResponse] =
    a => produce(Json.stringify(a.json))

  implicit lazy val consumeAccountDetails: Consume[AccountDetails] =
    consume[String].fmap(Json.parse).fmap(AccountDetails)
  implicit lazy val produceAccountDetails: Produce[AccountDetails] =
    a => produce(Json.stringify(a.json))

  implicit lazy val consumeProfileRef: Consume[ProfileRef] = consume[UserId].fmap(ProfileRef)
  implicit lazy val produceProfileRef: Produce[ProfileRef] = a => produce(a.userId)

  implicit lazy val consumeProfileData: Consume[ProfileData] =
    (consume[AuthenticationProvider] and
      consume[ProviderUserId] and
      consume[ProviderResponse] and
      consume[Email] and
      consume[Option[Password]] and
      consume[AccountDetails])(ProfileData)

  implicit lazy val produceProfileData: Produce[ProfileData] =
    a => produce(a.provider) andThen
      produce(a.providerUserId) andThen
      produce(a.email) andThen
      produce(a.password) andThen
      produce(a.accountDetails)

  implicit lazy val consumeProfile: Consume[Profile] =
    (consume[ProfileId] and consume[ProfileRef] and consume[ProfileData])(Profile)
  implicit lazy val produceProfile: Produce[Profile] =
    a => produce(a.id) andThen produce(a.ref) andThen produce(a.data)

  implicit lazy val consumeResourceId: Consume[ResourceId] = consume[String].fmap(ResourceId)
  implicit lazy val consumeresourceIdOption: Consume[Option[ResourceId]] = consume[Option[String]].fmap(_.map(ResourceId))
  implicit lazy val produceResourceId: Produce[ResourceId] = a => produce(a.value)
  implicit lazy val produceResourceIdOption: Produce[Option[ResourceId]] = a => produce(a.map(_.value))

  implicit lazy val consumeResourceType: Consume[ResourceUri] = consume[String].fmap(ResourceUri)
  implicit lazy val consumeResourceTypeOption: Consume[Option[ResourceUri]] = consume[Option[String]].fmap(_.map(ResourceUri))
  implicit lazy val produceResourceType: Produce[ResourceUri] = a => produce(a.value)
  implicit lazy val produceResourceTypeOption: Produce[Option[ResourceUri]] = a => produce(a.map(_.value))

  implicit lazy val consumeResource: Consume[Resource] = (consume[ResourceId] ~ consume[ResourceUri])(Resource)
  implicit lazy val produceResource: Produce[Resource] = a => produce(a.resourceId) andThen produce(a.resourceUri)
  implicit lazy val consumeResourceOption: Consume[Option[Resource]] =
    (consume[Option[ResourceId]] ~ consume[Option[ResourceUri]]) { (rido, rto) =>
      for {
        rid <- rido
        rt <- rto
      } yield Resource(rid, rt)
    }
  implicit lazy val produceResourceOption: Produce[Option[Resource]] =
    a => produce(a.map(_.resourceId)) andThen produce(a.map(_.resourceUri))

  implicit lazy val consumePermission: Consume[Permission] = consume[String].fmap(r => Permission(Integer.parseUnsignedInt(r, 2)))
  implicit lazy val producePermission: Produce[Permission] = a => produce(a.code)

  implicit lazy val consumeGroupId: Consume[GroupId] = consume[String].fmap(GroupId)
  implicit lazy val consumeGroupIdOption: Consume[Option[GroupId]] = consume[Option[String]].fmap(_.map(GroupId))
  implicit lazy val produceGroupId: Produce[GroupId] = a => produce(a.value)
  implicit lazy val produceGroupIdOption: Produce[Option[GroupId]] = a => produce(a.map(_.value))

  implicit lazy val consumeGroupRef: Consume[GroupRef] = (consume[UserId] ~ consume[Option[GroupId]])(GroupRef)
  implicit lazy val produceGroupRef: Produce[GroupRef] = a => produce(a.parent)

  implicit lazy val consumeCode: Consume[Code] = consume[String].fmap(Code)
  implicit lazy val produceCode: Produce[Code] = a => produce(a.value)

  implicit lazy val consumeGroupData: Consume[GroupData] = consume[Code].fmap(GroupData)
  implicit lazy val produceGroupData: Produce[GroupData] = a => produce(a.code)

  implicit lazy val consumeGroup: Consume[Group] =
    (consume[GroupId] ~ consume[GroupRef] ~ consume[GroupData])(Group)
  implicit lazy val produceGroup: Produce[Group] =
    a => produce(a.id) andThen produce(a.ref) andThen produce(a.data)

  implicit lazy val consumeGroupMemberRef: Consume[GroupMemberRef] =
    (consume[GroupId] ~ consume[SubjectId])(GroupMemberRef)
  implicit lazy val produceGroupMemberRef: Produce[GroupMemberRef] =
    a => produce(a.groupId) andThen produce(a.memberId)

  implicit lazy val consumeGroupMember: Consume[GroupMember] =
    consume[GroupMemberRef].fmap(GroupMember)
  implicit lazy val produceGroupMember: Produce[GroupMember] =
    a => produce(a.ref)

  implicit lazy val consumeAclId: Consume[AclId] = consume[String].fmap(AclId)
  implicit lazy val produceAclId: Produce[AclId] = a => produce(a.value)

  implicit lazy val consumeAclData: Consume[AclData] =
    (consume[Subject] ~ consume[ResourceUri] ~ consume[Permission])(AclData)
  implicit lazy val produceAclData: Produce[AclData] =
    a => produce(a.subject) andThen produce(a.resourceUri) andThen produce(a.permission)

  implicit lazy val consumeAcl: Consume[Acl] = (consume[AclId] ~ consume[AclData])(Acl)
  implicit lazy val produceAcl: Produce[Acl] = a => produce(a.id) andThen produce(a.data)

  implicit lazy val consumeRoleId: Consume[RoleId] = consume[String].fmap(RoleId)
  implicit lazy val produceRoleId: Produce[RoleId] = a => produce(a.value)

  implicit lazy val consumeRoleData: Consume[RoleData] = (consume[Code] ~ consume[Permission])(RoleData)
  implicit lazy val produceRoleData: Produce[RoleData] = a => produce(a.code) andThen produce(a.permission)

  implicit lazy val consumeRole: Consume[Role] = (consume[RoleId] ~ consume[RoleData])(Role)
  implicit lazy val produceRole: Produce[Role] = a => produce(a.id) andThen produce(a.data)

}
