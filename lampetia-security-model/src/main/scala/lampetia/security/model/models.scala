package lampetia.security.model

import lampetia.model._
import scala.util.{Success, Try}


case class SubjectId(value: String) extends AnyVal

case class UserId(value: String) extends AnyVal
case class User(id: UserId)

case class ProviderId(value: String) extends AnyVal
case class ProviderUserId(value: String) extends AnyVal
case class ProviderResponse(value: JSON) extends AnyVal
case class UserDetails(value: JSON) extends AnyVal
case class ProfileId(value: String) extends AnyVal
case class ProfileRef(userId: UserId)
case class ProfileData
  (providerId: ProviderId,
   providerUserId: ProviderUserId,
   email: Email,
   providerResponse: ProviderResponse,
   userDetails: UserDetails)
case class Profile(id: ProfileId, ref: ProfileRef, data: ProfileData)

sealed trait SubjectType
case object SubjectUser extends SubjectType
case object SubjectGroup extends SubjectType
case object SubjectApplication extends SubjectType
case object SubjectSystem extends SubjectType
object SubjectType {
  def asSubjectType(value: String) = value match {
    case "USER"        => SubjectUser
    case "GROUP"       => SubjectGroup
    case "APPLICATION" => SubjectApplication
    case "SYSTEM"      => SubjectSystem
  }

  def asString(instance: SubjectType) = instance match {
    case SubjectUser        => "USER"
    case SubjectGroup       => "GROUP"
    case SubjectApplication => "APPLICATION"
    case SubjectSystem      => "SYSTEM"
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


object UserModel
  extends Model[User]
  with HasId[User, UserId]
  with CanCombine1[User, UserId]
  with UUIDGenerator {

  val name: String = "User"
  def generate: UserId = UserId(generateStringId)
  def parse(stringId: String): Try[UserId] = Success(UserId(stringId))
  val id: Property[User, UserId] = property[UserId]("id")(_.id)(e => v => e.copy(id = v))
  def combine(id: UserId): User = User(id)

  override val features: Seq[Feature] = Seq(
    sql.name("security_user")
  )
}

object ProfileModel
  extends Model[Profile]
  with HasId[Profile, ProfileId]
  with HasRef[Profile, ProfileRef]
  with HasData[Profile, ProfileData]
  with CanCombine3[Profile, ProfileId, ProfileRef, ProfileData]
  with UUIDGenerator {
  val name: String = "Profile"
  def generate: ProfileId = ProfileId(generateStringId)
  def parse(stringId: String): Try[ProfileId] = Success(ProfileId(stringId))
  val id: Property[Profile, ProfileId] = property("id")(_.id)(e => v => e.copy(id = v))
  object ref extends RefModel[Profile, ProfileRef] {
    val userId: Property[ProfileRef, UserId] = property("userId")(_.userId)(e => v => e.copy(userId = v))
    def get(instance: Profile): ProfileRef = instance.ref
    def set(instance: Profile, value: ProfileRef): Profile = instance.copy(ref = value)
    val properties: Seq[Property[ProfileRef, _]] = Seq(userId)
  }
  object data extends DataModel[Profile, ProfileData] {
    val providerId: Property[ProfileData, ProviderId] =
      property("providerId")(_.providerId)(e => v => e.copy(providerId = v))
    val providerUserId: Property[ProfileData, ProviderUserId] =
      property("providerUserId")(_.providerUserId)(e => v => e.copy(providerUserId = v))
    val providerResponse: Property[ProfileData, ProviderResponse] =
      property("providerResponse")(_.providerResponse)(e => v => e.copy(providerResponse = v))
        .set(sql.`type`("jsonb"))
    val email: Property[ProfileData, Email] =
      property("email")(_.email)(e => v => e.copy(email = v))
    val userDetails: Property[ProfileData, UserDetails] =
      property("userDetails")(_.userDetails)(e => v => e.copy(userDetails = v))
        .set(sql.`type`("jsonb"))
    def get(instance: Profile): ProfileData = instance.data
    def set(instance: Profile, value: ProfileData): Profile = instance.copy(data = value)
    val properties: Seq[Property[ProfileData, _]] =
      Seq(providerId, providerUserId, providerResponse, userDetails)
  }
  def combine(a1: ProfileId, a2: ProfileRef, a3: ProfileData): Profile = Profile(a1,a2,a3)

  override val features: Seq[Feature] = Seq(
    sql.name("security_profile"),
    sql.primaryKey("security_profile_pk")(id),
    sql.foreignKey("security_profile_user_id_ref_user")(ref.userId)(UserModel.id),
    sql.uniqueIndex("security_profile_udx_1")(data.providerId, data.email)
  )
}

object GroupModel
  extends Model[Group]
  with HasId[Group, GroupId]
  with HasRef[Group, GroupRef]
  with HasData[Group, GroupData]
  with CanCombine3[Group, GroupId, GroupRef, GroupData]
  with UUIDGenerator {
  val name: String = "Group"
  def generate: GroupId = GroupId(generateStringId)
  def parse(stringId: String): Try[GroupId] = Success(GroupId(stringId))
  val id: Property[Group, GroupId] = property("id")(_.id)(e => v => e.copy(id = v))
  object ref extends RefModel[Group, GroupRef] {
    val parent: Property[GroupRef, Option[GroupId]] =
      property("parent")(_.parent)(e => v => e.copy(parent = v))
    def get(instance: Group): GroupRef = instance.ref
    def set(instance: Group, value: GroupRef): Group = instance.copy(ref = value)
    val properties: Seq[Property[GroupRef, _]] = Seq(parent)
  }
  object data extends DataModel[Group, GroupData] {
    val code: Property[GroupData, Code] =
      property("code")(_.code)(e => v => e.copy(code = v))
    def get(instance: Group): GroupData = instance.data
    def set(instance: Group, value: GroupData): Group = instance.copy(data = value)
    val properties: Seq[Property[GroupData, _]] = Seq(code)
  }
  def combine(a1: GroupId, a2: GroupRef, a3: GroupData): Group = Group(a1,a2,a3)
}

object GroupMemberModel
  extends Model[GroupMember]
  with HasRef[GroupMember, GroupMemberRef]
  with CanCombine1[GroupMember, GroupMemberRef] {
  val name: String = "GroupMember"
  object ref extends RefModel[GroupMember, GroupMemberRef] {
    val groupId: Property[GroupMemberRef, GroupId] =
      property("groupId")(_.groupId)(e => v => e.copy(groupId = v))
    val memberId: Property[GroupMemberRef, SubjectId] =
      property("memberId")(_.memberId)(e => v => e.copy(memberId = v))
    def get(instance: GroupMember): GroupMemberRef = instance.ref
    def set(instance: GroupMember, value: GroupMemberRef): GroupMember = instance.copy(ref = value)
    val properties: Seq[Property[GroupMemberRef, _]] = Seq(groupId, memberId)
  }

  def combine(a1: GroupMemberRef): GroupMember = GroupMember(a1)
  override val features: Seq[Feature] = Seq(
    sql.name("security_group_member")
  )
}

object RoleModel
  extends Model[Role]
  with HasId[Role, RoleId]
  with HasData[Role, RoleData]
  with CanCombine2[Role, RoleId, RoleData]
  with UUIDGenerator {
  val name: String = "Role"
  def generate: RoleId = RoleId(generateStringId)
  def parse(stringId: String): Try[RoleId] = Success(RoleId(stringId))
  val id: Property[Role, RoleId] = property("id")(_.id)(e => v => e.copy(id = v))
  object data extends DataModel[Role, RoleData] {
    val code: Property[RoleData, Code] =
      property("code")(_.code)(e => v => e.copy(code = v))
    val permission: Property[RoleData, Permission] =
      property("permission")(_.permission)(e => v => e.copy(permission = v))
        .set(sql.`type`("bit(32"))
    def get(instance: Role): RoleData = instance.data
    def set(instance: Role, value: RoleData): Role = instance.copy(data = value)
    def properties: Seq[Property[RoleData, _]] = Seq(code, permission)
  }
  def combine(a1: RoleId, a2: RoleData): Role = Role(a1, a2)
  override val features: Seq[Feature] = Seq(
    sql.name("security_role")
  )
}

object AclModel
  extends Model[Acl]
  with HasId[Acl, AclId]
  with HasData[Acl, AclData]
  with CanCombine2[Acl, AclId, AclData]
  with UUIDGenerator {
  val name = "Acl"
  val id: Property[Acl, AclId] = property("id")(_.id)(e => v => e.copy(id = v))
  def generate: AclId = AclId(generateStringId)
  def parse(stringId: String): Try[AclId] = Success(AclId(stringId))
  object data extends DataModel[Acl, AclData] {
    object subject extends Composite[AclData, Subject] {
      val subjectId: Property[Subject, SubjectId] = 
        property("subjectId")(_.subjectId)(e => v => e.copy(subjectId = v))
      val subjectType: Property[Subject, SubjectType] = 
        property("subjectType")(_.subjectType)(e => v => e.copy(subjectType = v))
      def get(instance: AclData): Subject = instance.subject
      def set(instance: AclData, value: Subject): AclData = instance.copy(subject = value)
      val properties: Seq[Property[Subject, _]] = Seq(subjectId, subjectType)
    }
    object resource extends Composite[AclData, Resource] {
      val resourceId: Property[Resource, ResourceId] =
        property("resourceId")(_.resourceId)(e => v => e.copy(resourceId = v))
      val resourceType: Property[Resource, ResourceType] =
        property("resourceType")(_.resourceType)(e => v => e.copy(resourceType = v))
      def get(instance: AclData): Resource = instance.resource
      def set(instance: AclData, value: Resource): AclData = instance.copy(resource = value)
      val properties: Seq[Property[Resource, _]] = Seq(resourceId, resourceType)
    }
    object parentResource extends Composite[AclData, Option[Resource]] {
      val resourceId: Property[Option[Resource], Option[ResourceId]] =
        resource.resourceId.liftOption
          .set(sql.optional)
          .set(sql.name("parent_resource_id"))
      val resourceType: Property[Option[Resource], Option[ResourceType]] =
        resource.resourceType.liftOption
          .set(sql.optional)
          .set(sql.name("parent_resource_type"))
      def get(instance: AclData): Option[Resource] = instance.parentResource
      def set(instance: AclData, value: Option[Resource]): AclData = instance.copy(parentResource = value)
      val properties: Seq[Property[_, _]] = Seq(resourceId, resourceType)
    }
    val permission: Property[AclData, Permission] =
      property("permission")(_.permission)(e => v => e.copy(permission = v))
        .set(sql.`type`("bit(32)"))
    def get(instance: Acl): AclData = instance.data
    def set(instance: Acl, value: AclData): Acl = instance.copy(data = value)
    val properties: Seq[Property[_, _]] =
      subject.properties ++ resource.properties ++ parentResource.properties :+ permission
  }

  def combine(a1: AclId, a2: AclData): Acl = Acl(a1,a2)

  override val features: Seq[Feature] = Seq(
    sql.name("security_subject_grant")
  )
}

object SecuritySqlFormat {
  import lampetia.sql.dsl._
  
  implicit lazy val consumeSubjectId: Consume[SubjectId] = consume[String].fmap(SubjectId)
  implicit lazy val produceSubjectId: Produce[SubjectId] = a => produce(a.value)

  implicit lazy val consumeSubjectType: Consume[SubjectType] = consume[String].fmap(SubjectType.asSubjectType)
  implicit lazy val produceSubjectType: Produce[SubjectType] = a => produce(SubjectType.asString(a))

  implicit lazy val consumeSubject: Consume[Subject] = (consume[SubjectId] ~ consume[SubjectType])(Subject)
  implicit lazy val produceSubject: Produce[Subject] = a => produce(a.subjectId) andThen produce(a.subjectType)

  implicit lazy val consumeUserId: Consume[UserId] = consume[String].fmap(UserId)
  implicit lazy val produceUserId: Produce[UserId] = a => produce(a.value)

  implicit lazy val consumeUser: Consume[User] = consume[UserId].fmap(User)
  implicit lazy val produceUser: Produce[User] = a => produce(a.id)

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
  import lampetia.sql.dsl._
  import scala.concurrent.Await
  import scala.concurrent.duration.Duration
  import SecuritySqlFormat._
  import scala.concurrent.ExecutionContext.Implicits.global

  def run[A](io: SqlIO[A]): Unit = {
    val f = io.run
    f.onSuccess { case v => println(v) }
    f.onFailure { case e => println(e) }
    Await.ready(f, Duration.Inf)
  }

  implicit val context: ConnectionSource =
    hikari(
      "org.postgresql.ds.PGSimpleDataSource",
      "localhost", 5432, "jeelona", "admin", "admin", 3, 2000)


  val u = UserModel
  val acl = AclModel
  val s = 'jeelona

  import lampetia.sql.dialect.postgres._
  val q = createTable(acl).sqlString
  println(q.sqlString)

  //val f = q.lifted.readSqlIO[Acl]

  //run(f)

  //context.shutdown()

}


















