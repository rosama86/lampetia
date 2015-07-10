package lampetia.security.service

import lampetia.security.model._
import lampetia.sql.dialect.postgres.jdbc._

/**
 * @author Radwa Osama
 */
trait RoleService {

  import lampetia.security.module.SecurityModule._

  protected def insertRole(role: Role): IO[Int] = {
    val rm = RoleModel
    rm.insert(rm.id := role.id.bind,
      rm.data.code := role.data.code.bind,
      rm.data.permission := role.data.permission.bind.cast(Types.bit(32)))
  }

  def createRole(data: RoleData): IO[Role] = {
    val rm  = RoleModel
    val role = Role(rm.generate, data)
    insertRole(role)
      .transactionally
      .map(_ => role)
  }

  def findRoleByRoleId(id: RoleId): IO[Option[Role]] = {
    val rm = RoleModel
    select(rm.properties: _*)
      .from(rm.schemaPrefixed)
      .where(rm.id === id.bind)
      .lifted
      .read[Role]
      .map(_.headOption)
  }

  def findAll(max: Int): IO[Seq[Role]] = {
    val rm = RoleModel
    select(rm.properties: _*)
      .from(rm.schemaPrefixed)
      .limit(max.bind)
      .lifted
      .read[Role]
  }

  def removeRole(roleId: RoleId): IO[Int] = {
    val rm = RoleModel
    deleteFrom(rm.schemaPrefixed)
      .where(rm.id === roleId.bind)
      .lifted
      .write
      .transactionally
  }

}
