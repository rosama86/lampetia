package lampetia.security.service

import java.util.UUID

import lampetia.model._
import lampetia.security.model._
import lampetia.security.module.SecurityTestModule._
import lampetia.test.LampetiaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.Json

import scala.util.Random

/**
 * @author Hossam Karim
 */

class UserServiceSpec extends FlatSpec with Matchers with ScalaFutures with LampetiaFutures {

  implicit val ec = executionContext

  val service = new UserService {}

  it should "create user" in {
    val email = s"${UUID.randomUUID.toString}@test.org"
    def profileData =
      ProfileData(
        UsernamePasswordProvider,
        ProviderUserId(""),
        ProviderResponse(PlayJson(Json.parse("[]"))),
        Email(email),
        Password("unsafe"),
        AccountDetails(PlayJson(Json.parse("[]"))),
        AccountActive)
    val user = service.createUser(profileData).run
    whenReady(user, oneMinute) { result =>
      result.id.value shouldNot be('empty)
    }
  }

  it should "create user profile" in {
    def email = s"${UUID.randomUUID.toString}@test.org"
    def profileData =
      ProfileData(
        UsernamePasswordProvider,
        ProviderUserId(""),
        ProviderResponse(PlayJson(Json.parse("[]"))),
        Email(email),
        Password("unsafe"),
        AccountDetails(PlayJson(Json.parse("[]"))),
        AccountActive)
    val actions = for {
      u <- service.createUser(profileData)
      p <- service.createUserProfile(u.id, profileData)
    } yield p

    val f = actions.transactionally.run

    whenReady(f, oneMinute) { result =>
      result.id.value shouldNot be('empty)
    }
  }








}
