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

  // Test grant(data: AclData): IO[Acl]
  it must "create a new ACL" in {

    When("assigning permission on a group to a user ")
    val createNewAcl = newAcl.run

    whenReady(createNewAcl, oneMinute) { result =>
      Then("generated acl Id must not be empty")
      result.id.value shouldNot be(EMPTY)
    }
  }

  it should "find an existing ACL records for a subject on a resource" in {

    When("finding a non existing ACL, it shouldn't be found")
    val nonExistingAcl =
      service.findAcl(SubjectId("acl.data.subject.subjectId"), ResourceUri("acl.data.resourceUri")).run

    whenReady(nonExistingAcl, oneMinute) { result =>
      Then("it shouldn't be found")
      result should be(None)
    }

    val findNewAcl =
      newAcl.flatMap {
        acl => service.findAcl(acl.data.subject.subjectId, acl.data.resourceUri)
      }.run

    When("finding an existing ACL, it shouldn't be found")
    whenReady(findNewAcl, oneMinute) { result =>
      Then("it should be found")
      result shouldNot be(None)
    }
  }

  it should "find ACL record by Id" in {

    When("finding a non existing ACL, it shouldn't be found")
    val nonExistingAcl = service.findAcl(AclId("non-existing-id")).run

    whenReady(nonExistingAcl, oneMinute) { result =>
      Then("it shouldn't be found")
      result should be(None)
    }

    val findNewAcl =
      newAcl.flatMap { acl => service.findAcl(acl.id)}.run

    When("finding an existing ACL, it shouldn't be found")
    whenReady(findNewAcl, oneMinute) { result =>
      Then("it should be found")
      result shouldNot be(None)
    }

  }

  it should "find all ACL" in {
    // add multiple Acls
    val addMultipleAcls =
      newAcl.flatMap(_ => newAcl).flatMap(_ => service.findAll(10)).run

    whenReady(addMultipleAcls, oneMinute) { result =>
      Then("at least 2 acls should be there")
    //  result shouldNot be(Nil)
   //   result.size should be > 2
    }
  }

  it should "revoke all subject permissions on resource, given ACL id" in {
    true should be(false)
  }

  it should "revoke all subject permissions on resource, given a resource and a subject" in {
    true should be(false)
  }

  it should "revoke a specific permission given to a subject on resource" in {
    true should be(false)
  }

  it should "revoke all permission given to a subject on any resource" in {
    true should be(false)
  }

  it should "have permission" in {
    true should be(false)
  }

  private def newAcl = {
    userService.createUser(testProfileData)
      .flatMap {
      owner =>
        owner.id.value shouldNot be(EMPTY)
        def groupData = GroupData(code = Code(UUID.randomUUID.toString))
        groupService.createGroup(groupRef(owner.id), groupData)

    }.flatMap {
      group =>
        group.id.value shouldNot be(EMPTY)
        val subject = Subject(SubjectId(group.ref.owner.value), SubjectUser)
        val resource = Resource(ResourceId(group.id.value), ResourceUri("com.nxt.group"))
        val aclData = AclData(subject, resource.resourceUri.*, rootPermission)
        service.grant(aclData)
    }
  }
}