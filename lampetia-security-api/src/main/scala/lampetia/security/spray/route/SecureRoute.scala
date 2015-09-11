package lampetia.security.spray.route

import lampetia.security.model.UserId
import org.slf4j.LoggerFactory
import spray.http.HttpHeaders.Authorization
import spray.http.OAuth2BearerToken
import spray.routing._
import spray.routing.directives.AuthMagnet

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
 * @author rhelal
 */
trait SecureRoute extends HttpService {

  import lampetia.security.module.SecurityModule
  import AuthMagnet._
  import spray.routing.authentication._
  import spray.util._

  private val logger = LoggerFactory.getLogger("Security")

  implicit def executionContext: ExecutionContext

  def authenticator(rc: RequestContext): Future[Authentication[UserId]] = {
    val authHeader: Option[Authorization] = rc.request.headers.findByType[`Authorization`]
    authHeader match {
      case Some(Authorization(OAuth2BearerToken(token))) =>
        Authenticator.userId(token) match {
          case Failure(e) =>
            logger.error("Failed during Authenticator userId validation", e)
            val reason =
              AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsRejected, Nil)
            if(logger.isDebugEnabled) {
              logger.debug(s"Access Denied: ${e.getMessage}")
            }
            Future.successful(Left(reason))

          case Success(userId) =>
            if(logger.isDebugEnabled) {
              logger.debug("Access Granted")
            }
            Future.successful(Right(UserId(userId)))

        }
      case Some(_) | None =>
        val reason =
          AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsRejected, Nil)
        if(logger.isDebugEnabled) {
          logger.debug("Access Denied: No Authorize header")
        }
        Future.successful(Left(reason))
    }
  }


  def secure(router: UserId => Route): Route =
    authenticate[UserId]((rc: RequestContext) => authenticator(rc))(router)

}