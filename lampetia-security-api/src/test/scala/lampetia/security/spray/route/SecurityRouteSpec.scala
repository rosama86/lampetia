package lampetia.security.spray.route

import java.util.UUID

import akka.actor.ActorSystem
import lampetia.model.Email
import lampetia.security.model._
import lampetia.security.module.SecurityTestModule
import lampetia.security.service.UserService
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Minutes, Span}
import org.scalatest.{Matchers, FlatSpec}
import play.api.libs.json.Json
import spray.http.HttpHeaders.Authorization
import spray.http.{StatusCodes, OAuth2BearerToken}
import spray.testkit.ScalatestRouteTest
import spray.httpx.PlayJsonSupport._

/**
 * @author rhelal
 */
class SecurityRouteSpec extends FlatSpec with Matchers  with ScalaFutures with ScalatestRouteTest {

  import SecurityTestModule.json._
  implicit val connectionSource = SecurityTestModule.connectionSource

  val service = new UserService {}

  val securityRoute = new SecurityRoute {
    def actorRefFactory: ActorSystem = SecurityTestModule.configuration.akka.defaultActorSystem
  }

  val route = securityRoute.validateRoute

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

  it should "GET user by user id to validate it" in {
    val futureUser = service.createUser(testProfileData).run
    whenReady(futureUser, oneMinute) { user =>
      val token = Authenticator.compact(user.id.value)
      val header = Authorization(OAuth2BearerToken(token))
      val headers = List(header)
      Get("/validate").withHeaders(headers) ~> route ~> check {

        val expected = user
        val response = responseAs[User]

        status should be(StatusCodes.OK)
        response should be(expected)
      }
    }
  }

  /*it should "GET user by not valid user id to validate it" in {
    val token = Authenticator.compact("test")
    val header = Authorization(OAuth2BearerToken(token))
    val headers = List(header)
    Get("/validate").withHeaders(headers) ~> route ~> check {
      status should be(StatusCodes.Unauthorized)
    }
  }

  it should "GET user by user id without security token to validate it" in {
    val futureUser = service.createUser(testProfileData).run
    whenReady(futureUser, oneMinute) { user =>
      val headers = List.empty[Authorization]
      Get("/validate").withHeaders(headers) ~> route ~> check {
        status should be(StatusCodes.Unauthorized)
      }
    }
  }*/

}
