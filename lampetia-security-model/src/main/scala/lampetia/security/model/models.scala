package lampetia.security.model

import lampetia.model._
import play.api.libs.json.Json
import scala.util.{Success, Try}


case class SubjectId(value: String) extends AnyVal

case class UserId(value: String) extends AnyVal
case class User(id: UserId)


trait SecureModel[E] extends Model[E]

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

case class ProviderUserId(value: String) extends AnyVal
case class ProviderResponse(value: JSON) extends AnyVal
case class AccountDetails(value: JSON) extends AnyVal
case class ProfileId(value: String) extends AnyVal
case class Password(value: String) extends AnyVal
case class ProfileRef(userId: UserId)
case class ProfileData(
   provider: AuthenticationProvider,
   providerUserId: ProviderUserId,
   providerResponse: ProviderResponse,
   email: Email,
   password: Password,
   accountDetails: AccountDetails,
   accountState: AccountState)
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
case class GroupRef(parent: Option[GroupId]) extends AnyVal
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

case class RoleId(value: String) extends AnyVal
case class RoleData(code: Code, permission: Permission)
case class Role(id: RoleId, data: RoleData)

case class AclId(value: String) extends AnyVal
case class AclData(subject: Subject, resource: Resource, parentResource: Option[Resource], permission: Permission)
case class Acl(id: AclId, data: AclData)
case class AclRoleRef(aclId: AclId, roleId: RoleId)
case class AclRole(ref: AclRoleRef) extends AnyVal

object SecurityModel {

  implicit object UserModel
    extends Model[User]
    with HasId[User, UserId]
    with CanBuild0[User]
    with CanBuild1[User, UserId]
    with CanGenerate[UserId]
    with CanParse[UserId]
    with UUIDGenerator {

    val modelName: String = "User"
    val id = property[UserId]("id")
    def generate: UserId = UserId(generateStringId)
    def parse(stringId: String): Try[UserId] = Success(UserId(stringId))
    def build: BuildResult[User] = BuildSuccess(User(generate))
    def build(id: UserId): BuildResult[User] = BuildSuccess(User(id))

    override val features: Seq[Feature] = Seq(
      sql.schema("sec"),
      sql.name("security_user"),
      sql.primaryKey(id)
    )
  }

  implicit object ProfileModel
    extends Model[Profile]
    with HasId[Profile, ProfileId]
    with HasRef[Profile, ProfileRef]
    with HasData[Profile, ProfileData]
    with CanBuild2[Profile, ProfileRef, ProfileData]
    with CanBuild3[Profile, ProfileId, ProfileRef, ProfileData]
    with CanGenerate[ProfileId]
    with CanParse[ProfileId]
    with UUIDGenerator {
    val modelName: String = "Profile"
    val id = property[ProfileId]("id")
    def generate: ProfileId = ProfileId(generateStringId)
    def parse(stringId: String): Try[ProfileId] = Success(ProfileId(stringId))
    object ref extends RefModel[ProfileRef] {
      val userId = property[UserId]("userId")
      val properties = Seq(userId)
    }
    object data extends DataModel[ProfileData] {
      val provider = property[AuthenticationProvider]("provider")
      val providerUserId = property[ProviderUserId]("providerUserId")
      val providerResponse = property[ProviderResponse]("providerResponse").set(sql.`type`("jsonb"))
      val email = property[Email]("email")
      val password = property[Password]("password")
      val accountDetails = property[AccountDetails]("accountDetails").set(sql.`type`("jsonb"))
      val accountState = property[AccountState]("accountState")
      val properties = Seq(provider, providerUserId, providerResponse, email, password, accountDetails, accountState)
    }
    def build(ref: ProfileRef, data: ProfileData): BuildResult[Profile] =
      BuildSuccess(Profile(generate, ref, data))
    def build(id: ProfileId, ref: ProfileRef, data: ProfileData): BuildResult[Profile] =
      BuildSuccess(Profile(id, ref, data))

    override val features: Seq[Feature] = Seq(
      sql.schema("sec"),
      sql.name("security_profile"),
      sql.primaryKey("security_profile_pk")(id),
      sql.foreignKey("security_profile_user_id_ref_user")(ref.userId)(UserModel, UserModel.id),
      sql.index("security_profile_user_id_idx")(ref.userId),
      sql.index("security_profile_state_idx")(data.accountState),
      sql.uniqueIndex("security_profile_provider_email_uidx")(data.provider, data.email)
    )
  }

  implicit object GroupModel
    extends Model[Group]
    with HasId[Group, GroupId]
    with HasRef[Group, GroupRef]
    with HasData[Group, GroupData]
    with CanBuild2[Group, GroupRef, GroupData]
    with CanBuild3[Group, GroupId, GroupRef, GroupData]
    with CanGenerate[GroupId]
    with CanParse[GroupId]
    with UUIDGenerator {
    val modelName: String = "Group"
    val id = property[GroupId]("id")
    def generate: GroupId = GroupId(generateStringId)
    def parse(stringId: String): Try[GroupId] = Success(GroupId(stringId))
    object ref extends RefModel[GroupRef] {
      val parent = property[Option[GroupId]]("parent").set(sql.optional)
      val properties = Seq(parent)
    }
    object data extends DataModel[GroupData] {
      val code = property[Code]("code")
      val properties = Seq(code)
    }
    def build(id: GroupId, ref: GroupRef, data: GroupData): BuildResult[Group] = BuildSuccess(Group(id, ref, data))
    def build(ref: GroupRef, data: GroupData): BuildResult[Group] = BuildSuccess(Group(generate, ref, data))
    override val features: Seq[Feature] = Seq(
      sql.schema("sec"),
      sql.name("security_group"),
      sql.primaryKey("security_group_pk")(id)
    )
  }

  implicit object GroupMemberModel
    extends Model[GroupMember]
    with HasRef[GroupMember, GroupMemberRef]
    with CanBuild1[GroupMember, GroupMemberRef] {
    val modelName: String = "GroupMember"
    object ref extends RefModel[GroupMemberRef] {
      val groupId = property[GroupId]("groupId")
      val memberId = property[SubjectId]("memberId")
      val properties = Seq(groupId, memberId)
    }

    def build(ref: GroupMemberRef): BuildResult[GroupMember] = BuildSuccess(GroupMember(ref))

    override val features: Seq[Feature] = Seq(
      sql.schema("sec"),
      sql.name("security_group_member"),
      sql.foreignKey("sgm_ref_group_id")(ref.groupId)(GroupModel, GroupModel.id),
      sql.foreignKey("sgm_ref_user_id")(ref.memberId)(UserModel, UserModel.id)
    )
  }

  implicit object RoleModel
    extends Model[Role]
    with HasId[Role, RoleId]
    with HasData[Role, RoleData]
    with CanBuild2[Role, RoleId, RoleData]
    with CanGenerate[RoleId]
    with CanParse[RoleId]
    with UUIDGenerator {
    val modelName: String = "Role"
    val id = property[RoleId]("id")
    def generate: RoleId = RoleId(generateStringId)
    def parse(stringId: String): Try[RoleId] = Success(RoleId(stringId))
    object data extends DataModel[RoleData] {
      val code = property[Code]("code")
      val permission = property[Permission]("permission").set(sql.`type`("bit(32"))
      val properties = Seq(code, permission)
    }
    def build(a1: RoleId, a2: RoleData): BuildResult[Role] = BuildSuccess(Role(a1, a2))
    override val features: Seq[Feature] = Seq(
      sql.schema("sec"),
      sql.name("security_role")
    )
  }

  implicit object AclModel
    extends Model[Acl]
    with HasId[Acl, AclId]
    with HasData[Acl, AclData]
    with CanBuild2[Acl, AclId, AclData]
    with CanGenerate[AclId]
    with CanParse[AclId]
    with UUIDGenerator {
    val modelName = "Acl"
    val id = property[AclId]("id")
    def generate: AclId = AclId(generateStringId)
    def parse(stringId: String): Try[AclId] = Success(AclId(stringId))
    object data extends DataModel[AclData] {
      object subject extends Composite[Subject] {
        val subjectId = property[SubjectId]("subjectId")
        val subjectType = property[SubjectType]("subjectType")
        val properties = Seq(subjectId, subjectType)
      }
      object resource extends Composite[Resource] {
        val resourceId = property[ResourceId]("resourceId")
        val resourceType = property[ResourceType]("resourceType")
        val properties = Seq(resourceId, resourceType)
      }
      object parentResource extends Composite[Option[Resource]] {
        val resourceId =
          property[Option[ResourceId]]("resourceId")
            .set(sql.optional)
            .set(sql.name("parent_resource_id"))
        val resourceType =
          property[ResourceType]("resourceType")
            .set(sql.optional)
            .set(sql.name("parent_resource_type"))
        val properties = Seq(resourceId, resourceType)
      }
      val permission = property[Permission]("permission").set(sql.`type`("bit(32)"))
      val properties = subject.properties ++ resource.properties ++ parentResource.properties :+ permission
    }

    def build(a1: AclId, a2: AclData): BuildResult[Acl] = BuildSuccess(Acl(a1,a2))

    override val features: Seq[Feature] = Seq(
      sql.schema("sec"),
      sql.name("security_subject_grant")
    )
  }
}

import lampetia.sql.dialect.h2.jdbc._

object SecuritySqlFormat {

  implicit lazy val consumeSubjectId: Consume[SubjectId] = consume[String].fmap(SubjectId)
  implicit lazy val produceSubjectId: Produce[SubjectId] = a => produce(a.value)

  implicit lazy val consumeSubjectType: Consume[SubjectType] = consume[String].fmap(SubjectType.apply)
  implicit lazy val produceSubjectType: Produce[SubjectType] = a => produce(a.value)

  implicit lazy val consumeSubject: Consume[Subject] = (consume[SubjectId] ~ consume[SubjectType])(Subject)
  implicit lazy val produceSubject: Produce[Subject] = a => produce(a.subjectId) andThen produce(a.subjectType)

  implicit lazy val consumeUserId: Consume[UserId] = consume[String].fmap(UserId)
  implicit lazy val produceUserId: Produce[UserId] = a => produce(a.value)

  implicit lazy val consumeUser: Consume[User] = consume[UserId].fmap(User)
  implicit lazy val produceUser: Produce[User] = a => produce(a.id)

  implicit lazy val consumeAuthenticationProvider: Consume[AuthenticationProvider] = consume[String].fmap(AuthenticationProvider.apply)
  implicit lazy val produceAuthenticationProvider: Produce[AuthenticationProvider] = a => produce(a.value)

  implicit lazy val consumeProfileId: Consume[ProfileId] = consume[String].fmap(ProfileId)
  implicit lazy val produceProfileId: Produce[ProfileId] = a => produce(a.value)

  implicit lazy val consumeProviderUserId: Consume[ProviderUserId] = consume[String].fmap(ProviderUserId)
  implicit lazy val produceProviderUserId: Produce[ProviderUserId] = a => produce(a.value)

  implicit lazy val consumeEmail: Consume[Email] = consume[String].fmap(Email)
  implicit lazy val produceEmail: Produce[Email] = a => produce(a.value)

  implicit lazy val consumePassword: Consume[Password] = consume[String].fmap(Password)
  implicit lazy val producePassword: Produce[Password] = a => produce(a.value)

  implicit lazy val consumeAccountState: Consume[AccountState] = consume[String].fmap(AccountState.apply)
  implicit lazy val produceAccountState: Produce[AccountState] = a => produce(a.value)

  implicit lazy val consumeProviderResponse: Consume[ProviderResponse] =
    consume[String].fmap(json => PlayJson(Json.parse(json))).fmap(ProviderResponse)
  implicit lazy val produceProviderResponse: Produce[ProviderResponse] =
    a => produce(a.value.stringify)

  implicit lazy val consumeAccountDetails: Consume[AccountDetails] =
    consume[String].fmap(json => PlayJson(Json.parse(json))).fmap(AccountDetails)
  implicit lazy val produceAccountDetails: Produce[AccountDetails] =
    a => produce(a.value.stringify)

  implicit lazy val consumeProfileRef: Consume[ProfileRef] = consume[UserId].fmap(ProfileRef)
  implicit lazy val produceProfileRef: Produce[ProfileRef] = a => produce(a.userId)

  implicit lazy val consumeProfileData: Consume[ProfileData] =
    (consume[AuthenticationProvider] and
     consume[ProviderUserId] and
     consume[ProviderResponse] and
     consume[Email] and
     consume[Password] and
     consume[AccountDetails] and
     consume[AccountState])(ProfileData)
  implicit lazy val produceProfileData: Produce[ProfileData] =
    a => produce(a.provider) andThen
         produce(a.providerUserId) andThen
         produce(a.email) andThen
         produce(a.password) andThen
         produce(a.accountDetails) andThen
         produce(a.accountState)

  implicit lazy val consumeProfile: Consume[Profile] =
    (consume[ProfileId] and consume[ProfileRef] and consume[ProfileData])(Profile)
  implicit lazy val produceProfile: Produce[Profile] =
   a => produce(a.id) andThen produce(a.ref) andThen produce(a.data)

  implicit lazy val consumeResourceId: Consume[ResourceId] = consume[String].fmap(ResourceId)
  implicit lazy val consumeresourceIdOption: Consume[Option[ResourceId]] = consume[Option[String]].fmap(_.map(ResourceId))
  implicit lazy val produceResourceId: Produce[ResourceId] = a => produce(a.value)
  implicit lazy val produceResourceIdOption: Produce[Option[ResourceId]] = a => produce(a.map(_.value))

  implicit lazy val consumeResourceType: Consume[ResourceType] = consume[String].fmap(ResourceType)
  implicit lazy val consumeResourceTypeOption: Consume[Option[ResourceType]] = consume[Option[String]].fmap(_.map(ResourceType))
  implicit lazy val produceResourceType: Produce[ResourceType] = a => produce(a.value)
  implicit lazy val produceResourceTypeOption: Produce[Option[ResourceType]] = a => produce(a.map(_.value))

  implicit lazy val consumeResource: Consume[Resource] = (consume[ResourceId] ~ consume[ResourceType])(Resource)
  implicit lazy val produceResource: Produce[Resource] = a => produce(a.resourceId) andThen produce(a.resourceType)
  implicit lazy val consumeResourceOption: Consume[Option[Resource]] =
    (consume[Option[ResourceId]] ~ consume[Option[ResourceType]]) { (rido, rto) =>
       for {
         rid <- rido
         rt <- rto
       } yield Resource(rid, rt)
    }
  implicit lazy val produceResourceOption: Produce[Option[Resource]] =
    a => produce(a.map(_.resourceId)) andThen produce(a.map(_.resourceType))

  implicit lazy val consumePermission: Consume[Permission] = consume[Int].fmap(Permission)
  implicit lazy val producePermission: Produce[Permission] = a => produce(a.code)

  implicit lazy val consumeGroupId: Consume[GroupId] = consume[String].fmap(GroupId)
  implicit lazy val consumeGroupIdOption: Consume[Option[GroupId]] = consume[Option[String]].fmap(_.map(GroupId))
  implicit lazy val produceGroupId: Produce[GroupId] = a => produce(a.value)
  implicit lazy val produceGroupIdOption: Produce[Option[GroupId]] = a => produce(a.map(_.value))

  implicit lazy val consumeGroupRef: Consume[GroupRef] = consume[Option[GroupId]].fmap(GroupRef)
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
    (consume[Subject] ~ consume[Resource] ~ consume[Option[Resource]] ~ consume[Permission])(AclData)
  implicit lazy val produceAclData: Produce[AclData] =
    a => produce(a.subject) andThen produce(a.resource) andThen produce(a.parentResource) andThen produce(a.permission)

  implicit lazy val consumeAcl: Consume[Acl] = (consume[AclId] ~ consume[AclData])(Acl)
  implicit lazy val produceAcl: Produce[Acl] = a => produce(a.id) andThen produce(a.data)
}


object SecurityModelTest extends App {
  import scala.concurrent.Await
  import scala.concurrent.duration.Duration
  import SecurityModel._
  import SecuritySqlFormat._
  import scala.concurrent.ExecutionContext.Implicits.global

  def run[A](io: IO[A]): Unit = {
    val f = io.run
    f.onSuccess { case v => println(v) }
    f.onFailure { case e => e.printStackTrace() }
    Await.ready(f, Duration.Inf)
  }

  implicit lazy val context: ConnectionSource = {
    val ds = new org.h2.jdbcx.JdbcDataSource
    ds.setUrl("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1")
    connectionSource(ds)
  }


  val u = UserModel
  val acl = AclModel
  val g = GroupModel
  val gm = GroupMemberModel
  val s = 'jeelona

  val q = for {
    _ <- "create schema sec".sql.write
    _ <- u.create
    _ <- g.create
    _ <- gm.create
    i <- g.insert(GroupRef(None), GroupData(Code("abc")))
    _ <- g.insert(GroupRef(None), GroupData(Code("xyz")))
    _ <- g.update(g.data.code := Code("123").bind)(g.id === i.id.bind)
    x <- g.find//(g.id === i.id.bind)
  } yield x

  run(q.transactionally)

  gm.insert(GroupMemberRef(GroupId(""), SubjectId("")))

  /*u.createSql.foreach(println)
  println("------------")
  g.createSql.foreach(println)
  println("------------")
  gm.createSql.foreach(println)
  println("------------")*/


  context.shutdown()

}


















