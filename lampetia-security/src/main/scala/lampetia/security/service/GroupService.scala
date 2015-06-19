package lampetia.security.service

import lampetia.security.model._
import lampetia.sql.dialect.postgres.jdbc._

/**
 * @author Radwa Osama
 */
trait GroupService {

  import lampetia.security.module.SecurityModule._

  protected def insertGroup(group: Group): IO[Int] = {
    val m = GroupModel
    m.insert(m.id := group.id.bind,
      m.data.code := group.data.code.bind,
      m.ref.parent := group.ref.parent.bind)
  }

  protected def insertGroupMember(groupId: GroupId, memberId: UserId): IO[Int] = {
    val gm = GroupMemberModel
    gm.insert(gm.ref.groupId := groupId.bind,
      gm.ref.memberId := memberId.bind)
  }

  def createGroup(data: GroupData, parent: Option[GroupId] = None): IO[Group] = {
    val g = GroupModel
    val group = Group(g.generate, GroupRef(parent), data)
    insertGroup(group)
      .transactionally
      .map(_ => group)
  }

  def findGroupByGroupId(id: GroupId): IO[Option[Group]] = {
    val g = GroupModel
    select(g.properties: _*)
      .from(g.schemaPrefixed)
      .where(g.id === id.bind)
      .lifted
      .read[Group]
      .map(_.headOption)
  }

  def findGroupByParentGroupId(id: GroupId): IO[Option[Group]] = {
    val g = GroupModel
    select(g.properties: _*)
      .from(g.schemaPrefixed)
      .where(g.ref.parent === id.bind)
      .lifted
      .read[Group]
      .map(_.headOption)
  }

  def findAll(max: Int): IO[Seq[Group]] = {
    val g = GroupModel
    select(g.properties: _*)
      .from(g.schemaPrefixed)
      .limit(max.bind)
      .lifted
      .read[Group]
  }

  def addMember(groupId: GroupId, memberId: UserId): IO[Int] = {
    insertGroupMember(groupId, memberId)
      .transactionally
  }

  def removeMember(groupId: GroupId, memberId: UserId): IO[Int] = {
    val gm = GroupMemberModel
    deleteFrom(gm.schemaPrefixed)
      .where((gm.ref.groupId === groupId.bind) and (gm.ref.memberId === memberId.bind))
      .lifted
      .write
  }

  def removeGroup(groupId: GroupId): IO[Int] = {
    // remove all members
    val gm = GroupMemberModel
    deleteFrom(gm.schemaPrefixed)
      .where(gm.ref.groupId === groupId.bind)
      .lifted
      .write

    // remove group
    val g = GroupModel
    deleteFrom(g.schemaPrefixed)
      .where(g.id === groupId.bind)
      .lifted
      .write
  }

}
