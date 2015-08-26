package lampetia.security.spray.route

import lampetia.security.model.{UserId, User}
import lampetia.security.service.UserService
import spray.http.StatusCodes
import spray.routing.HttpService

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * @author rhelal
 */
trait SecurityRoute extends HttpService with SecureRoute {

  import lampetia.security.module.SecurityModule

  import SecurityModule.json._
  import spray.httpx.PlayJsonSupport._

  /*
  POST      /group  --> create new group without parent
  POST      /group/$id --> create new group with parent group of id = $id

  GET       /group/$id --> find group by group id = $id
  DELETE    /group/$id --> delete group by group id = $id

  PUT       /group/$id/member/$id --> update group of first id to add member of second id on it
  DELETE    /group/$id/member/$id --> update group of first id to remove member of second id on it

   */

  implicit val executionContext = SecurityModule.configuration.concurrent.executionContext

  val userService = new UserService {}

  def findOneById(userId: UserId): Future[Option[User]] =
    userService.findOne(userId).run

  def validateRoute =
    head {
      path("validate") {
        secure { userId =>
          complete(StatusCodes.OK)
        }
      }
    } ~
      get {
        path("validate") {
          secure { userId =>
            onComplete(findOneById(userId)) {
              case Success(Some(user)) =>
                complete(user)
              case Success(None) =>
                complete(StatusCodes.NotFound)
              case Failure(e) =>
                failWith(e)
            }
          }
        }
      }

}
