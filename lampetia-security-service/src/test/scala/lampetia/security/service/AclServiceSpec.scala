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

  it should "not find a non existing Acl for a subject on a resource" in {
    val nonExistingAcl =
      service.findAcl(SubjectId("acl.data.subject.subjectId"), ResourceUri("acl.data.resourceUri")).run

    whenReady(nonExistingAcl, oneMinute) { result =>
      result should be(None)
    }
  }

  it should "find an existing ACL records for a subject on a resource" in {
    val findNewAcl =
      newAcl.flatMap {
        acl => service.findAcl(acl.data.subject.subjectId, acl.data.resourceUri)
      }.run

    When("finding an existing ACL, it shouldn't be found")
    whenReady(findNewAcl, oneMinute) { result =>
      result shouldNot be(None)
    }
  }

  it should "return false when checking a non existing ACL" in {
    val nonExistingAcl =
      service.hasAcl(SubjectId("acl.data.subject.subjectId"), ResourceUri("acl.data.resourceUri")).run
    whenReady(nonExistingAcl, oneMinute) { result =>
      result should be(right = false)
    }
  }

  it should "return true when checking a non existing ACL" in {
    val findNewAcl =
      newAcl.flatMap {
        acl => service.hasAcl(acl.data.subject.subjectId, acl.data.resourceUri)
      }.run
    whenReady(findNewAcl, oneMinute) { result =>
      result should be(right = true)
    }
  }

  it should "not find ACL record with non existing Id" in {
    val nonExistingAcl = service.findAcl(AclId("non-existing-id")).run
    whenReady(nonExistingAcl, oneMinute) { result =>
      result should be(None)
    }
  }

  it should "find ACL record by Id" in {
    val findNewAcl =
      newAcl.flatMap { acl => service.findAcl(acl.id)}.run

    whenReady(findNewAcl, oneMinute) { result =>
      result shouldNot be(None)
    }
  }

  it should "find all ACL" in {
    // add multiple Acls
    val addMultipleAcls =
      newAcl.flatMap(_ => newAcl).flatMap(_ => service.findAll(10)).run

    whenReady(addMultipleAcls, oneMinute) { result =>
      result shouldNot be(Nil)
      result.size should be > 2
    }
  }

  it should "not add a role to a subject on a resource if no ACL records exist" in {
    val grantRole =
      service.grant(
        SubjectId("invalidSubject"), ResourceUri("invalid-resource"), RoleId("invalid-role")).run

    whenReady(grantRole, oneMinute) { result =>
      result should be(right = false)
    }
  }

  it should " add a role to a subject on a resource if ACL records exist" in {
    val grantRole =
      newAcl.
        flatMap {
        acl =>
          roleService.createRole(RoleData(Code("test-role"), updatePermission))
            .flatMap {
            role =>
              service.grant(acl.data.subject.subjectId, acl.data.resourceUri, role.id)
          }
      }.run

    whenReady(grantRole, oneMinute) { result =>
      result should be(right = true)
    }
  }

  it should "not add permission to a subject on a resource if no ACL records exists" in {
    val grantPermission =
      service.grant(
        SubjectId("invalidSubject"), ResourceUri("invalid-resource"), createPermission).run

    whenReady(grantPermission, oneMinute) { result =>
      result should be(right = false)
    }
  }

  it should "add permission to a subject on a resource if ACL records exists" in {
    val grantPermission =
      newAcl.
        flatMap {
        acl =>
          service.grant(acl.data.subject.subjectId, acl.data.resourceUri, createPermission)
      }.run

    whenReady(grantPermission, oneMinute) { result =>
      result should be(right = true)
    }
  }

  it should "have no permission by default" in {

    whenReady(newAcl.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val resourceUri = acl.data.resourceUri

      // it should have no permission
      val noPermission =
        service.hasPermission(subjectId,
          resourceUri, readPermission | createPermission | updatePermission | deletePermission | rootPermission).run

      whenReady(noPermission, oneMinute) { result =>
        result should be (right = false)
      }
    }
  }

  it should "have only read permission inherited from ACL" in {

    whenReady(newAcl.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val resourceUri = acl.data.resourceUri

      whenReady(service.grant(subjectId, resourceUri, readPermission).run, oneMinute) { granted =>

        // it should have no permission
        val noPermission =
          service.hasPermission(subjectId, resourceUri,
            createPermission | updatePermission | deletePermission | rootPermission).run

        whenReady(noPermission, oneMinute) { result =>
          result should be (right = false)
        }

        // it should have read permission
        val readPermissionR =
          service.hasPermission(subjectId, resourceUri, readPermission).run

        whenReady(readPermissionR, oneMinute) { result =>
          result should be (right = true)
        }
      }
    }
  }

  it should "have only read permission inherited from role" in {

    whenReady(newAcl.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val resourceUri = acl.data.resourceUri

      val addRole =
      roleService.createRole(RoleData(Code("test-role"), readPermission)).flatMap {
        role =>
          service.grant(subjectId, resourceUri, role.id)
      }.run

      whenReady(addRole, oneMinute) { granted =>
        // it should have no permission from:
        val noPermission =
          service.hasPermission(subjectId, resourceUri,
            createPermission | updatePermission | deletePermission | rootPermission).run

        whenReady(noPermission, oneMinute) { result =>
          result should be(right = false)
        }

        // it should have read permission
        val readPermissionR =
          service.hasPermission(subjectId, resourceUri, readPermission).run

        whenReady(readPermissionR, oneMinute) { result =>
          result should be(right = true)
        }
      }
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
        val aclData = AclData(subject, resource.resourceUri.*, noPermission)
        service.grant(aclData)
    }
  }
}