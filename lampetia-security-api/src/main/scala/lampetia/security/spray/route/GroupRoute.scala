package lampetia.security.spray.route

import lampetia.security.model._
import lampetia.security.service.GroupService
import spray.http.StatusCodes
import spray.routing.HttpService
import scala.util.{Failure, Success}
import spray.httpx.PlayJsonSupport._

/**
 * @author rhelal
 */
trait GroupRoute extends HttpService with SecureRoute {

  import lampetia.security.module.SecurityModule

  import SecurityModule.json._
  import SecurityModule.sql._

  /*
  POST      /group  --> create new group without parent
  POST      /group/$parentGroupId --> create new group with parent group of id = $parentGroupId

  GET       /group/$groupId --> find group by group id = $groupId
  DELETE    /group/$groupId --> delete group by group id = $groupId

  POST       /group/$groupId/member/$userId --> add member of $userId to group of $groupId
  DELETE    /group/$groupId/member/$userId --> remove member of $userId from group of $groupId

   */

  implicit val executionContext = SecurityModule.configuration.concurrent.executionContext

  val groupService = new GroupService {}

  def postGroup(owner: UserId, parentGroupId: Option[String], data: GroupData) = {
    val ref = GroupRef(owner, parentGroupId.map(GroupId(_)))
    groupService.createGroup(ref, data).run
  }

  def groupRoute = {
    pathPrefix("group") {
      secure { userId =>
        pathEndOrSingleSlash {
          post {
            entity(as[GroupData]) { data =>
              onComplete(postGroup(userId, None, data)) {
                case Success(instance) =>
                  complete(StatusCodes.Created, instance)
                case Failure(err) => failWith(err)
              }
            }
          }
        } ~
          path(Segment) { groupId =>
            pathEndOrSingleSlash {
              post {
                entity(as[GroupData]) { data =>
                  onComplete(postGroup(userId, Some(groupId), data)) {
                    case Success(instance) =>
                      complete(StatusCodes.Created, instance)
                    case Failure(err) => failWith(err)
                  }
                }
              } ~
              get {
                onComplete(groupService.findOne(GroupId(groupId)).run) {
                  case Success(instance) => complete(instance)
                  case Failure(err) => failWith(err)
                }
              } ~
                delete {
                  onComplete(groupService.removeGroup(GroupId(groupId)).run) {
                    case Success(0) => complete(StatusCodes.NotFound)
                    case Success(_) => complete(StatusCodes.OK)
                    case Failure(err) => failWith(err)
                  }
                }
            }
          } ~
          path(Segment / "member" / Segment) { (groupId, memberId) =>
            post {
              onComplete(groupService.addMember(GroupId(groupId), UserId(memberId)).run) {
                case Success(0) => complete(StatusCodes.NotFound)
                case Success(_) => complete(StatusCodes.Created)
                case _ => complete(StatusCodes.NotFound)
              }
            } ~
              delete {
                onComplete(groupService.removeMember(GroupId(groupId), UserId(memberId)).run) {
                  case Success(0) => complete(StatusCodes.NotFound)
                  case Success(_) => complete(StatusCodes.OK)
                  case _ => complete(StatusCodes.NotFound)
                }
              }
          }
      }
    }
  }
}
