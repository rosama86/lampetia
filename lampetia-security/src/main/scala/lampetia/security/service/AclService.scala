package lampetia.security.service

import lampetia.security.model._
import lampetia.sql.dialect.postgres.jdbc._


/**
 * @author Radwa Osama
 */
trait AclService {

  import lampetia.security.module.SecurityModule._

  protected def insertAcl(acl: Acl): IO[Int] = {
    val aclm = AclModel
    acl.data.parentResource match {
      case None =>
        aclm.insert(aclm.id := acl.id.bind,
          aclm.data.permission := acl.data.permission.bind.cast(Types.bit(32)),
          aclm.data.subject.subjectId := acl.data.subject.subjectId.bind,
          aclm.data.subject.subjectType := acl.data.subject.subjectType.bind,
          aclm.data.resource.resourceId := acl.data.resource.resourceId.bind,
          aclm.data.resource.resourceType := acl.data.resource.resourceType.bind)
      case Some(pr) =>
        aclm.insert(aclm.id := acl.id.bind,
          aclm.data.permission := acl.data.permission.bind.cast(Types.bit(32)),
          aclm.data.subject.subjectId := acl.data.subject.subjectId.bind,
          aclm.data.subject.subjectType := acl.data.subject.subjectType.bind,
          aclm.data.resource.resourceId := acl.data.resource.resourceId.bind,
          aclm.data.resource.resourceType := acl.data.resource.resourceType.bind,
          aclm.data.parentResource.resourceId := acl.data.parentResource.get.resourceId.bind,
          aclm.data.parentResource.resourceType := acl.data.parentResource.get.resourceType.bind
        )
    }

  }

  def grantAcl(data: AclData): IO[Acl] = {
    val aclm = AclModel
    val acl = Acl(aclm.generate, data)
    insertAcl(acl)
      .transactionally
      .map(_ => acl)
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
/*
  def hasPermission(permission: Permission): Boolean = {
    val bp = permission.bind.cast(Types.bit(32))
    val aclm = AclModel
    select(aclm.properties: _*)
      .from(aclm.schemaPrefixed)
      .where((bp & aclm.data.permission.cast(Types.bit(32))) === bp)
      .lifted
      .read[Acl]
  }*/
}
