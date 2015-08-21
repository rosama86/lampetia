package lampetia.security.service

import java.util.UUID

import lampetia.model._
import lampetia.security.model._
import lampetia.security.module.SecurityTestModule._
import lampetia.test.LampetiaFutures
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

/**
 * @author Radwa Osama
 */
class AclServiceSpec extends FlatSpec with Matchers with GivenWhenThen with ScalaFutures with LampetiaFutures with CommonServiceSpec {

  import sql._

  implicit val ec = configuration.concurrent.executionContext

  val service = new AclService {}
  val groupService = new GroupService {}
  val userService = new UserService {}
  val roleService = new RoleService {}

  it must "create a new ACL" in {

    When("group owner creation starts ... ")
    val cur = userService.createUser(testProfileData).run
    whenReady(cur, oneMinute) { owner =>
      Then("generated group owner Id must not be empty")
      owner.id.value shouldNot be(EMPTY)

      When("group creation starts ... ")
      def groupData = GroupData(code = Code(UUID.randomUUID.toString))
      val cgr = groupService.createGroup(groupRef(owner.id), groupData).run

      whenReady(cgr, oneMinute) { group =>
        Then("generated group Id must not be empty")
        group.id.value shouldNot be(EMPTY)

        When("assigning group owner \"root\" permission on all group resources")
        val subject = Subject(SubjectId(owner.id.value), SubjectUser)
        val resource = Resource(ResourceId(group.id.value), ResourceUri("com.nxt.group"))
        val aclData = AclData(subject, resource.resourceUri.*, rootPermission)
        val acl = service.grant(aclData).run

        whenReady(acl, oneMinute) { result =>
          Then("generated acl Id must not be empty")
          result.id.value shouldNot be(EMPTY)
        }
      }
    }
  }

}