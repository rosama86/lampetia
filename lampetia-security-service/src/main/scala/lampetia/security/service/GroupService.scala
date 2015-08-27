package lampetia.security.service

import lampetia.security.model._

/**
 * @author Radwa Osama
 */
trait GroupService {

  import lampetia.security.module.SecurityModule.sql._

  protected def insertGroup(group: Group): IO[Int] = {
    val m = GroupModel
    m.insert(m.id := group.id.bind,
      m.data.code := group.data.code.bind,
      m.ref.owner := group.ref.owner.bind,
      m.ref.parent := group.ref.parent.bind)
  }

  protected def insertGroupMember(groupId: GroupId, memberId: UserId): IO[Int] = {
    val gm = GroupMemberModel
    gm.insert(gm.ref.groupId := groupId.bind,
      gm.ref.memberId := memberId.bind)
  }

  def createGroup(ref: GroupRef, data: GroupData): IO[Group] = {
    val g = GroupModel
    val group = Group(g.generate, ref, data)
    insertGroup(group)
      .transactionally
      .map(_ => group)
  }

  def findOne(id: GroupId): IO[Option[Group]] = {
    val g = GroupModel
    select(g.properties: _*)
      .from(g.schemaPrefixed)
      .where(g.id === id.bind)
      .lifted
      .read[Group]
      .map(_.headOption)
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
      .transactionally
  }

  def removeGroup(groupId: GroupId): IO[Int] = {
    // remove all members
    val gm = GroupMemberModel
    val dgm =
      deleteFrom(gm.schemaPrefixed)
        .where(gm.ref.groupId === groupId.bind)
        .lifted
        .write

    // remove group
    val g = GroupModel
    val dg =
      deleteFrom(g.schemaPrefixed)
        .where(g.id === groupId.bind)
        .lifted
        .write

    val q =
      for {
        _ <- dgm
        c <- dg
      } yield c

    q.transactionally
  }

  def findChildGroupsByParentGroupId(parentGroupId: GroupId): IO[Seq[Group]] = {
    val g = GroupModel
    val recursiveGroup = 'rec_group

    val columns = g.properties.map('g dot _)

    val withQuery =
      select(columns:_*)
        .from(g.schemaPrefixed as 'g)
        .where('g dot g.id === parentGroupId.bind)
      .union(
        select(columns:_*)
        .from(g.schemaPrefixed as 'g, recursiveGroup)
        .where(recursiveGroup dot 'id === 'g dot g.ref.parent))

    val selection =
      select(g.properties:_*)
      .from(recursiveGroup)

    withRecursive(recursiveGroup, withQuery, selection)
    .lifted
    .read[Group]
  }

}
