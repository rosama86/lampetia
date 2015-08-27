package lampetia.security.spray.route

import java.util.UUID

import akka.actor.ActorSystem
import lampetia.model.{Code, Email}
import lampetia.security.model._
import lampetia.security.spray.module.SecurityTestModule
import lampetia.security.module.SecurityModule.configuration
import lampetia.security.service.{UserService, GroupService}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Minutes, Span}
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.Json
import spray.http.HttpHeaders.Authorization
import spray.http.{StatusCodes, OAuth2BearerToken}
import spray.testkit.ScalatestRouteTest
import spray.httpx.PlayJsonSupport._


/**
 * @author rhelal
 */
class GroupRouteSpec extends FlatSpec with Matchers  with ScalaFutures with ScalatestRouteTest {

  import SecurityTestModule.json._

  val userService = new UserService {}

  val service = new GroupService {}

  val groupRoute = new GroupRoute {
    def actorRefFactory: ActorSystem = configuration.akka.defaultActorSystem
  }

  val route = groupRoute.groupRoute

  final val EMPTY = ""

  def groupRef(ownerId: UserId, parentGroupId: Option[GroupId] = None): GroupRef = GroupRef(ownerId, parentGroupId)

  def testProfileData = {
    val email = s"${UUID.randomUUID.toString}@test.org"
    ProfileData(
      UsernamePasswordProvider,
      ProviderUserId(""),
      ProviderResponse(Json.parse("[]")),
      Email(email),
      Some(Password("unsafe")),
      AccountDetails(Json.parse("[]")))
  }

  def oneMinute: Timeout = Timeout(Span(1, Minutes))

  it should "POST Group instance and return a Resource" in {
    val futureUser = userService.createUser(testProfileData).run
    whenReady(futureUser, oneMinute) { owner =>
      val groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val token = Authenticator.compact(owner.id.value)
      val header = Authorization(OAuth2BearerToken(token))
      val headers = List(header)
      Post("/group", groupData).withHeaders(headers) ~> route ~> check {
        status should be(StatusCodes.Created)

        val resourceId = responseAs[Group].id.value
        val expected = Group(GroupId(resourceId), groupRef(owner.id), groupData)
        val selectedGroup = service.findOne(GroupId(resourceId)).run
        whenReady(selectedGroup, oneMinute) { selection =>

          selection shouldNot be(None)
          selection.map(_.id.value) should be(Some(resourceId))
          selection should be(Some(expected))
        }
      }
    }
  }

  it should "POST Group instance with parent group and return a Resource" in {
    val futureUser = userService.createUser(testProfileData).run
    whenReady(futureUser, oneMinute) { owner =>
      val groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val futureParentGroup = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(futureParentGroup, oneMinute) { parentGroup =>
        parentGroup.id.value shouldNot be(EMPTY)

        // Add child group
        val childGroupData =
          GroupData(code = Code(UUID.randomUUID.toString))
        val token = Authenticator.compact(owner.id.value)
        val header = Authorization(OAuth2BearerToken(token))
        val headers = List(header)
        Post(s"/group/${parentGroup.id.value}", childGroupData).withHeaders(headers) ~> route ~> check {
          status should be(StatusCodes.Created)

          val resourceId = responseAs[Group].id.value
          val expected = Group(GroupId(resourceId), groupRef(owner.id, Some(parentGroup.id)), childGroupData)
          val selectedGroup = service.findOne(GroupId(resourceId)).run
          whenReady(selectedGroup, oneMinute) { selection =>

            selection shouldNot be(None)
            selection.map(_.id.value) should be(Some(resourceId))
            selection should be(Some(expected))
          }
        }
      }
    }
  }

  it should "GET Group instance by id" in {
    val futureUser = userService.createUser(testProfileData).run
    whenReady(futureUser, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val futureGroup = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(futureGroup, oneMinute) { group =>
        val token = Authenticator.compact(owner.id.value)
        val header = Authorization(OAuth2BearerToken(token))
        val headers = List(header)
        Get(s"/group/${group.id.value}").withHeaders(headers) ~> route ~> check {

          val expected = group
          val response = responseAs[Group]

          status should be(StatusCodes.OK)
          response should be(expected)
        }
      }
    }
  }

  it should "DELETE a Group" in {
    val futureUser = userService.createUser(testProfileData).run
    whenReady(futureUser, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val futureGroup = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(futureGroup, oneMinute) { group =>
        val token = Authenticator.compact(owner.id.value)
        val header = Authorization(OAuth2BearerToken(token))
        val headers = List(header)
        Delete(s"/group/${group.id.value}").withHeaders(headers) ~> route ~> check {
          status should be(StatusCodes.OK)

          val selectedGroup = service.findOne(group.id).run
          whenReady(selectedGroup, oneMinute) { selection =>
            selection should be(None)
          }
        }
      }
    }
  }

  it should "POST a Group member" in {
    val futureUser = userService.createUser(testProfileData).run
    whenReady(futureUser, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val futureGroup = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(futureGroup, oneMinute) { group =>
        val token = Authenticator.compact(owner.id.value)
        val header = Authorization(OAuth2BearerToken(token))
        val headers = List(header)
        Post(s"/group/${group.id.value}/member/${owner.id.value}").withHeaders(headers) ~> route ~> check {
          status should be(StatusCodes.Created)

          val selectedGroupMembers = service.findMembers(group.id).run
          whenReady(selectedGroupMembers, oneMinute) { groupMembers =>

            groupMembers shouldNot be(Seq.empty[GroupMember])
            groupMembers.size should be(1)
            groupMembers.map(_.ref.groupId) should be(List(group.id))
            //groupMembers.map(_.ref.memberId) should be(List(owner.id))
          }
        }
      }
    }
  }

  it should "DELETE a Group member" in {
    val futureUser = userService.createUser(testProfileData).run
    whenReady(futureUser, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val futureGroup = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(futureGroup, oneMinute) { group =>
        val futureResult = service.addMember(group.id, owner.id).run
        whenReady(futureResult, oneMinute) { result =>
          result should be(1)
          val token = Authenticator.compact(owner.id.value)
          val header = Authorization(OAuth2BearerToken(token))
          val headers = List(header)
          Delete(s"/group/${group.id.value}/member/${owner.id.value}").withHeaders(headers) ~> route ~> check {
            status should be(StatusCodes.OK)

            val selectedGroupMembers = service.findMembers(group.id).run
            whenReady(selectedGroupMembers, oneMinute) { groupMembers =>
              groupMembers should be(Seq.empty[GroupMember])
            }
          }
        }
      }
    }
  }

}
