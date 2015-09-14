package lampetia.security.spray.route

import lampetia.model.UserId
import lampetia.security.model.User
import lampetia.security.service.UserService
import lampetia.sql.JdbcConnectionSource
import spray.http.StatusCodes
import spray.routing.HttpService

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
 * @author rhelal
 */
trait SecurityRoute extends HttpService with SecureRoute {

  import lampetia.security.module.SecurityModule

  import SecurityModule.json._
  import spray.httpx.PlayJsonSupport._

  /*
  GET  /validate // secure, used for internal services to validate the tokens they have, like UI

   */

  implicit def executionContext: ExecutionContext
  implicit def connectionSource: JdbcConnectionSource

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
