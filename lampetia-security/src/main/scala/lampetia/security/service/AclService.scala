package lampetia.security.service

import lampetia.model.sql.ModelFeatures
import lampetia.model.{Resource, ResourceId}
import lampetia.security.model._
import lampetia.sql.dialect.postgres.jdbc._

/**
 * @author Radwa Osama
 */
trait AclService {

  import lampetia.security.module.SecurityModule._

  protected def insertAcl(acl: Acl): IO[Int] = {
    val aclm = AclModel

    val parentResourceCouples =
      acl.data.parentResource match {
        case None => Seq.empty
        case Some(parentResource) =>
          Seq(
            aclm.data.parentResource.resourceId  := parentResource.resourceId.bind,
            aclm.data.parentResource.resourceType := parentResource.resourceType.bind)
      }

    val couples =
      parentResourceCouples ++
      Seq(aclm.id := acl.id.bind,
        aclm.data.permission := acl.data.permission.bind.cast(Types.bit(32)),
        aclm.data.subject.subjectId := acl.data.subject.subjectId.bind,
        aclm.data.subject.subjectType := acl.data.subject.subjectType.bind,
        aclm.data.resource.resourceId := acl.data.resource.resourceId.bind,
        aclm.data.resource.resourceType := acl.data.resource.resourceType.bind)

    aclm.insert(couples : _*)
  }

  protected def insertAclRole(aclRole: AclRole): IO[Int] = {
    val aclrm = AclRoleModel
    aclrm.insert(aclrm.ref.aclId := aclRole.ref.aclId.bind,
      aclrm.ref.roleId := aclRole.ref.roleId.bind)
  }

  def grant(data: AclData): IO[Acl] = {
    val aclm = AclModel
    val acl = Acl(aclm.generate, data)
    insertAcl(acl)
      .transactionally
      .map(_ => acl)
  }

  def getAcl(subjectId: SubjectId, resource: Resource): IO[Option[AclId]] = {
    val aclm = AclModel
    select(aclm.id)
      .from(aclm.schemaPrefixed)
      .where(
        aclm.data.subject.subjectId === subjectId.bind and
          aclm.data.resource.resourceType === resource.resourceType.bind and
          aclm.data.resource.resourceType === resource.resourceType.bind)
      .lifted
      .read[AclId]
      .map(_.headOption)
  }

  def hasAcl(subjectId: SubjectId, resource: Resource): IO[Boolean] = {
    getAcl(subjectId, resource)
      .flatMap {
      case None => IO.pure(false)
      case _ => IO.pure(true)
    }
  }

  // adds roleId to subjectId on resourceId only if an Acl record already exists
  def grant(subjectId: SubjectId, resource: Resource, roleId: RoleId): IO[Boolean] = {
    val aclrm = AclRoleModel
    val aclm = AclModel

    insertInto(aclrm.schemaPrefixed).query(
      select(aclm.id, roleId.bind)
        .from(aclm.schemaPrefixed)
        .where(
          aclm.data.subject.subjectId === subjectId.bind and
            aclm.data.resource.resourceId === resource.resourceId.bind and
            aclm.data.resource.resourceType === resource.resourceType.bind)
    ).lifted.write.transactionally.map(_ => true)
  }

  // adds a permission to subjectId on resourceId only if ACL record already exists
  def grant(subject: Subject, resource: Resource, permission: Permission): IO[Boolean] = {
    val aclm = AclModel
    val filter =
      aclm.data.subject.subjectId === subject.subjectId.bind and
        aclm.data.subject.subjectType === subject.subjectType.bind and
        aclm.data.resource.resourceId === resource.resourceId.bind and
        aclm.data.resource.resourceType === resource.resourceType.bind

    select(aclm.properties: _*)
      .from(aclm.schemaPrefixed)
      .where(filter)
      .lifted
      .read[Acl]
      .flatMap {
      case Nil => IO.pure(0)
      case acl =>
        aclm.update(aclm.data.permission :=
          (permission | acl.head.data.permission).bind.cast(Types.bit(32)))(filter).transactionally
    }.flatMap {
      r => IO.pure(r > 0)
    }
  }

  def findAclByAclId(id: AclId): IO[Option[Acl]] = {
    val aclm = AclModel
    select(aclm.properties: _*)
      .from(aclm.schemaPrefixed)
      .where(aclm.id === id.bind)
      .lifted
      .read[Acl]
      .map(_.headOption)
  }

  def findAll(max: Int): IO[Seq[Acl]] = {
    val aclm = AclModel
    select(aclm.properties: _*)
      .from(aclm.schemaPrefixed)
      .limit(max.bind)
      .lifted
      .read[Acl]
  }

  def hasPermission(subjectId: SubjectId, resourceId: ResourceId, permission: Permission): IO[Boolean] = {
    select(
      function(hasPermissionFunctionName,
        subjectId.bind,
        resourceId.bind,
        permission.bind.cast(Types.bit(32))))
      .lifted
      .read[Boolean]
      .map(_.head)
  }

  def revokePermission(aclId: AclId): IO[Int] = {
    val aclm = AclModel
    deleteFrom(aclm.schemaPrefixed)
      .where(aclm.id === aclId.bind)
      .lifted
      .write
      .transactionally
  }

  def rvokePermission(subjectId: SubjectId, resourceId: ResourceId, permission: Permission): IO[Int] = {
    val aclm = AclModel
    deleteFrom(aclm.schemaPrefixed)
      .where((aclm.data.subject.subjectId === subjectId.bind) and
      (aclm.data.resource.resourceId === resourceId.bind) and
      (aclm.data.permission & permission.bind.cast(Types.bit(32)) === permission.bind.cast(Types.bit(32))))
      .lifted
      .write
      .transactionally
  }

  def revokePermission(subjectId: SubjectId, resourceId: ResourceId): IO[Int] = {
    val aclm = AclModel
    deleteFrom(aclm.schemaPrefixed)
      .where((aclm.data.subject.subjectId === subjectId.bind) and
      (aclm.data.resource.resourceId === resourceId.bind))
      .lifted
      .write
      .transactionally
  }

  def revokeAllPermissions(subjectId: SubjectId): IO[Int] = {
    val aclm = AclModel
    deleteFrom(aclm.schemaPrefixed)
      .where(aclm.data.subject.subjectId === subjectId.bind)
      .lifted
      .write
      .transactionally
  }

  val hasPermissionFunctionName = AclModel.sqlSchema match {
    case Some(prefix) => prefix + "." + "has_permission"
    case None => "has_permission"
  }
}
