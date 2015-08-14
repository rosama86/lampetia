package lampetia.security.sm

import java.util.UUID

import lampetia.model.{Code, ResourceUri}

/**
 * @author Hossam Karim
 */


object SecurityModelSpec extends App { self =>

  def uuid = UUID.randomUUID.toString

  def user(name: String) = User(UserId(name), AccountActive)
  def userId(name: String) = UserId(name)

  def group(owner: UserId, parent: Option[GroupId], code: String) =
    Group(GroupId(uuid), GroupRef(owner, parent), GroupData(Code(code)))
  
  def groupMember(groupId: GroupId, memberId: SubjectId) =
    GroupMember(GroupMemberRef(groupId, memberId))

  implicit class StringEx(val s: String) {
    def user = self.user(s)
    def userId = self.userId(s)
    def subjectId = SubjectId(s)
    def resourceUri = ResourceUri(s)
  }

  implicit class UserIdEx(val id: UserId) {
    def subject: Subject = Subject(SubjectId(id.value), SubjectUser)
  }

  implicit class UserEx(val user: User) {
    def subject: Subject = Subject(SubjectId(user.id.value), SubjectUser)
  }

  implicit class GroupIdEx(val id: GroupId) {
    def subject: Subject = Subject(SubjectId(id.value), SubjectGroup)
  }

  implicit class GroupEx(val group: Group) {
    def subject: Subject = Subject(SubjectId(group.id.value), SubjectGroup)
  }

  implicit class OptionEx[A](val any: A) {
    def asParent = Some(any)
  }


  def acl(subject: Subject, uri: String, permission: Permission) =
    Acl(AclId(uuid), AclData(subject, ResourceUri(uri), permission))

  val c1u1 = "c1u1".user
  val c1u2 = "c1u2".user
  val c1c2u0 = "c1c2u0".user
  val c2u1 = "c2u1".user
  val c2u2 = "c2u2".user
  val c2u3 = "c2u3".user
  val c2u4 = "c2u4".user
  val c2u5 = "c2u5".user

  val users = Seq(c1u1, c1u2, c1c2u0, c2u1, c2u2, c2u3, c2u4)

  val c1Group =
    group(c1u2.id, None, "c1-group")
  val nxtGroup =
    group(c1u1.id, None, "nxt-group")
  val nxtBoardGroup =
    group(c1u1.id, Some(nxtGroup.id), "nxt-board-group")
  val nxtTechGroup =
    group(c2u1.id, Some(nxtGroup.id), "nxt-tech-group")
  val oracleConfGroup =
    group(c2u1.id, Some(nxtGroup.id), "nxt-conference-group")

  val groups =
    Seq(c1Group, nxtGroup, nxtBoardGroup, nxtTechGroup, oracleConfGroup)

  val metraGroupMembers =
    Seq(c1u2, c1u1).map(u => groupMember(c1Group.id, u.subject.subjectId))

  val nxtGroupMembers =
    Seq(c2u4).map(u => groupMember(nxtGroup.id, u.subject.subjectId))

  val nxtBoardGroupMembers =
    Seq(c1u2, c1u1, c1c2u0).map(u => groupMember(nxtBoardGroup.id, u.subject.subjectId))

  val nxtTechGroupMembers =
    Seq(c2u1, c2u2, c2u3).map(u => groupMember(nxtTechGroup.id, u.subject.subjectId))

  val oracleConfGroupMembers =
    Seq(c2u2, c2u3, c2u5).map(u => groupMember(oracleConfGroup.id, u.subject.subjectId))

  val groupMembers: Seq[GroupMember] =
    metraGroupMembers ++ nxtGroupMembers ++ nxtBoardGroupMembers ++ nxtTechGroupMembers ++ oracleConfGroupMembers


  def page(name: String) = Page(PageId(uuid), PageData(name))
  def conference(name: String) = Conference(ConferenceId(uuid), ConferenceData(name))
  def agenda(conferenceId: ConferenceId, name: String) =
    Agenda(AgendaId(uuid), AgendaRef(conferenceId), AgendaData(name))
  def video(conferenceId: ConferenceId, name: String) =
    Video(VideoId(uuid), VideoRef(conferenceId), VideoData(name))
  def presentation(conferenceId: ConferenceId, name: String) =
    Presentation(PresentationId(uuid), PresentationRef(conferenceId), PresentationData(name))
  def document(pageId: PageId, name: String) =
    Document(DocumentId(uuid), DocumentRef(pageId), DocumentData(name))




  final val noPermission         = Permission( 0 )
  final val readPermission       = Permission( 1 << 0 )
  final val createPermission     = Permission( 1 << 1 )
  final val updatePermission     = Permission( 1 << 3 )
  final val deletePermission     = Permission( 1 << 4 )
  final val crudPermission       = createPermission | readPermission | updatePermission | deletePermission

  // Resources
  val mainPage = page("Site Main Page")

  val homePage = page("User Home Page")

  val metraPage = page("Metra Home Page")

  val nxtPage = page("Nextechnology Home Page")
  val welcomeDocument = document(nxtPage.id, "Welcome Document")

  val oracleConference = conference("Oracle Technology Conference")
  val oracleConferenceAgenda = agenda(oracleConference.id, "Agenda")
  val oracleConferenceKeynoteVideo = video(oracleConference.id, "Keynote Video")
  val oracleConferenceKeynontePresentation = presentation(oracleConference.id, "Keynote Presentation")

  val acls = Seq(
    // [ACL-01] metraGroup CAN read and create metraPage
    acl(c1Group.subject, metraPage.uri, readPermission | createPermission),
    // [ACL-02] nxtTechGroup CAN create, read, update and delete ANY resource under oracleConference resource
    acl(nxtTechGroup.subject, s"${oracleConference.uri}/.*", crudPermission)
  )

  def subjectGroups(subjectId: SubjectId): Seq[Group] = {
    val directGroups: Seq[GroupId] =
      groupMembers.filter(_.ref.memberId == subjectId).map(_.ref.groupId)
    val parentGroups: Seq[GroupId] =
      groups.filter(g => directGroups.contains(g.id)).map(_.ref.parent).collect {
        case Some(p) => p
      }
    val all = directGroups ++ parentGroups
    groups.filter(g => all.contains(g.id))
  }

  def isGroupMember(groupId: GroupId, subjectId: SubjectId): Boolean =
    subjectGroups(subjectId).exists { g =>
      g.id == groupId
    }
  
  def subjectCompatible(lhs: Subject, rhs: Subject) =
    lhs == rhs ||
      (lhs.subjectType == SubjectGroup &&
        isGroupMember(GroupId(lhs.subjectId.value), rhs.subjectId))

  def uriMatches(permitted: ResourceUri, test: ResourceUri): Boolean = {
    val permittedString = permitted.value
    val testString = test.value
    permittedString == testString || testString.matches(permittedString)
  }
  
  def hasPermission(subject: Subject, resourceUri: ResourceUri, mask: Permission) =
    acls.exists { a =>
      subjectCompatible(a.data.subject, subject) &&
      uriMatches(a.data.resourceUri, resourceUri) &&
      a.data.permission.exists(mask)
    }
  

  //assert(isGroupMember(nxtTechGroup.id, hossam.subject.subjectId))
  //assert(isGroupMember(nxtGroup.id, hossam.subject.subjectId))
  //assert(isGroupMember(nxtBoardGroup.id, badr.subject.subjectId))
  //assert(isGroupMember(nxtGroup.id, badr.subject.subjectId))

  // c1u2 CAN read metraPage [ACL-01]
  assert(hasPermission(c1u2.subject, metraPage.uri.resourceUri, readPermission))

  // c1u2 CAN create metraPage [ACL-01]
  assert(hasPermission(c1u2.subject, metraPage.uri.resourceUri, createPermission))

  // c1u2 CANNOT update metraPage [ACL-01]
  assert(!hasPermission(c1u2.subject, metraPage.uri.resourceUri, updatePermission))

  // c2u1 CANNOT read metraPage [ACL-01]
  assert(!hasPermission(c2u1.subject, metraPage.uri.resourceUri, readPermission))


  // c2u1 CAN delete oracle conference keynote video, since he's in the nxtTechGroup [ACL-02]
  assert(hasPermission(c2u1.subject, oracleConferenceKeynoteVideo.uri.resourceUri, deletePermission))







}
