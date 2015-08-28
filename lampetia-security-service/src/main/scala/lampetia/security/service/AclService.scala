package lampetia.security.service

import lampetia.model.{UriPattern, ResourceUri}
import lampetia.security.model._

/**
 * @author Radwa Osama
 */
trait AclService {

  import lampetia.security.module.SecurityModule.sql._

  protected def insertAcl(acl: Acl): IO[Int] = {
    val aclm = AclModel

    val couples =
      Seq(aclm.id := acl.id.bind,
        aclm.data.permission := acl.data.permission.bind.cast(Types.bit(32)),
        aclm.data.subject.subjectId := acl.data.subject.subjectId.bind,
        aclm.data.subject.subjectType := acl.data.subject.subjectType.bind,
        aclm.data.resourceUri := acl.data.resourceUri.bind)

    aclm.insert(couples: _*)
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

  // adds roleId to subjectId on resourceId only if an Acl record already exists
  def grant(subjectId: SubjectId, resourceUri: ResourceUri, roleId: RoleId): IO[Boolean] = {
    val aclrm = AclRoleModel
    val aclm = AclModel

    insertInto(aclrm.schemaPrefixed).query(
      select(aclm.id, roleId.bind)
        .from(aclm.schemaPrefixed)
        .where(
          aclm.data.subject.subjectId === subjectId.bind and
            aclm.data.resourceUri === resourceUri.bind)
    ).lifted.write.transactionally.map(_ > 0)
  }

  // adds a permission to subjectId on resourceId only if ACL record already exists
  def grant(subjectId: SubjectId, resourceUri: ResourceUri, permission: Permission): IO[Boolean] = {
    val aclm = AclModel
    val filter =
      aclm.data.subject.subjectId === subjectId.bind and
        aclm.data.resourceUri === resourceUri.bind

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

  def findOne(subjectId: SubjectId, resourceUri: ResourceUri): IO[Option[Acl]] = {
    val aclm = AclModel
    select(aclm.properties: _*)
      .from(aclm.schemaPrefixed)
      .where(
        aclm.data.subject.subjectId === subjectId.bind and
          aclm.data.resourceUri === resourceUri.bind)
      .lifted
      .read[Acl]
      .map(_.headOption)
  }

  def findOne(id: AclId): IO[Option[Acl]] = {
    val aclm = AclModel
    select(aclm.properties: _*)
      .from(aclm.schemaPrefixed)
      .where(aclm.id === id.bind)
      .lifted
      .read[Acl]
      .map(_.headOption)
  }

  def hasAcl(subjectId: SubjectId, resourceUri: ResourceUri): IO[Boolean] = {
    findOne(subjectId, resourceUri)
      .flatMap {
      case None => IO.pure(false)
      case _ => IO.pure(true)
    }
  }

  def findAll(max: Int): IO[Seq[Acl]] = {
    val aclm = AclModel
    select(aclm.properties: _*)
      .from(aclm.schemaPrefixed)
      .limit(max.bind)
      .lifted
      .read[Acl]
  }

  def revokePermission(aclId: AclId): IO[Int] = {
    deleteFrom(AclRoleModel.schemaPrefixed)
      .where(AclRoleModel.ref.aclId === aclId.bind)
      .lifted
      .write
      .flatMap {
      r =>
        deleteFrom(AclModel.schemaPrefixed)
          .where(AclModel.id === aclId.bind)
          .lifted
          .write
    }.transactionally
  }

  def revokePermission(subjectId: SubjectId, uriPattern: UriPattern, permission: Permission): IO[Int] = {
    val aclm = AclModel

    select(aclm.properties: _*)
      .from(aclm.schemaPrefixed)
      .where(
        aclm.data.subject.subjectId === subjectId.bind and
          aclm.data.resourceUri === uriPattern.bind)
      .lifted
      .read[Acl]
      .map(_.headOption)
      .flatMap {
      case None => IO.pure(0)
      case Some(acl) =>
        aclm.update(
          aclm.data.permission :=
            acl.data.permission.remove(permission).bind.cast(Types.bit(32)))(aclm.id === acl.id.bind).transactionally
    }
  }

  def revokeAllPermissions(subjectId: SubjectId, uriPattern: UriPattern): IO[Int] = {
    val aclm = AclModel
    deleteFrom(aclm.schemaPrefixed)
      .where((aclm.data.subject.subjectId === subjectId.bind) and
      (aclm.data.resourceUri ~ uriPattern.bind))
      .lifted
      .write
      .transactionally
  }

  def revokeAllPermissions(subjectId: SubjectId): IO[Int] = {
    val aclm = AclModel

    deleteFrom(AclRoleModel.schemaPrefixed)
      .where(
        AclRoleModel.ref.aclId in
          select(AclModel.id)
            .from(AclModel.schemaPrefixed)
            .where(AclModel.data.subject.subjectId === subjectId.bind)).lifted.write
      .flatMap {
        case _ =>
          deleteFrom(aclm.schemaPrefixed)
            .where(aclm.data.subject.subjectId === subjectId.bind)
            .lifted
            .write
    }.transactionally

  }

  def hasPermission(subjectId: SubjectId, uriPattern: UriPattern, permission: Permission): IO[Boolean] = {
    val aclm = AclModel

    select(
      function(has_permission.schemaPrefixed,
        subjectId.bind,
        uriPattern.bind,
        permission.bind.cast(Types.bit(32))))
      .lifted
      .read[Boolean]
      .map(_.head)
  }

}
