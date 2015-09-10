package lampetia.security.service

import lampetia.security.module.SecurityTestModule
import lampetia.test.LampetiaFutures
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

/**
 * @author Hossam Karim
 */

class UserServiceSpec extends FlatSpec with Matchers with ScalaFutures with LampetiaFutures with CommonServiceSpec {

  implicit val ec = SecurityTestModule.configuration.concurrent.executionContext
  implicit val connectionSource = SecurityTestModule.connectionSource

  val service = new UserService {}

  it should "create user" in {
    val user = service.createUser(testProfileData).run
    whenReady(user, oneMinute) { result =>
      result.id.value shouldNot be('empty)
    }
  }

  it should "create user profile" in {
    val actions = for {
      u <- service.createUser(testProfileData)
      p <- service.createUserProfile(u.id, testProfileData)
    } yield p

    val f = actions.transactionally.run

    whenReady(f, oneMinute) { result =>
      result.id.value shouldNot be('empty)
    }
  }

}
