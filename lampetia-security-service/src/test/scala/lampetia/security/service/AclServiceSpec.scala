package lampetia.security.service

import java.util.UUID

import lampetia.model._
import lampetia.security.model._
import lampetia.security.module.SecurityTestModule
import lampetia.test.LampetiaFutures
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

/**
 * @author Radwa Osama
 */
class AclServiceSpec extends FlatSpec with Matchers with GivenWhenThen with ScalaFutures with LampetiaFutures with CommonServiceSpec {

  import SecurityTestModule.sql._

  implicit val ec = SecurityTestModule.configuration.concurrent.executionContext
  implicit val connectionSource = SecurityTestModule.connectionSource

  val service = new AclService {}
  val groupService = new GroupService {}
  val userService = new UserService {}
  val roleService = new RoleService {}

  // Test grant(data: AclData): IO[Acl]
  it must "create a new ACL" in {

    When("assigning permission on a group to a user ")
    val createNewAcl = grantAclForAUserOnAGroup.run

    whenReady(createNewAcl, oneMinute) { result =>
      Then("generated acl Id must not be empty")
      result.id.value shouldNot be(EMPTY)
    }
  }

  it should "not find an Acl for a subject on a resource, if it was not granted one" in {
    val nonExistingAcl =
      service.findOne(SubjectId("acl.data.subject.subjectId"), ResourceUri("acl.data.resourceUri")).run

    whenReady(nonExistingAcl, oneMinute) { result =>
      result should be(None)
    }
  }

  it should "find an existing ACL records for a subject on a resource" in {
    val findNewAcl =
      grantAclForAUserOnAGroup.flatMap {
        acl => service.findOne(acl.data.subject.subjectId, acl.data.resourceUri)
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

  it should "return true when checking an existing ACL" in {
    val findExistingAcl =
      grantAclForAUserOnAGroup.flatMap {
        acl => service.hasAcl(acl.data.subject.subjectId, acl.data.resourceUri)
      }.run
    whenReady(findExistingAcl, oneMinute) { result =>
      result should be(right = true)
    }
  }

  it should "not find ACL record with invalid Id" in {
    val nonExistingAcl = service.findOne(AclId("non-existing-id")).run
    whenReady(nonExistingAcl, oneMinute) { result =>
      result should be(None)
    }
  }

  it should "find ACL record by Id" in {
    val findNewAcl =
      grantAclForAUserOnAGroup.flatMap { acl => service.findOne(acl.id)}.run

    whenReady(findNewAcl, oneMinute) { result =>
      result shouldNot be(None)
    }
  }

  it should "find all ACL" in {
    // add multiple Acls
    val addMultipleAcls =
      grantAclForAUserOnAGroup.flatMap(_ => grantAclForAUserOnAGroup).flatMap(_ => service.findAll(10)).run

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
      grantAclForAUserOnAGroup.
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
      grantAclForAUserOnAGroup.
        flatMap {
        acl =>
          service.grant(acl.data.subject.subjectId, acl.data.resourceUri, createPermission)
      }.run

    whenReady(grantPermission, oneMinute) { result =>
      result should be(right = true)
    }
  }

  it should "have no permission by default" in {

    whenReady(grantAclForAUserOnAGroup.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val uriPattern = UriPattern(acl.data.resourceUri.value)

      // it should have no permission
      val noPermission =
        service.hasPermission(subjectId,
          uriPattern, readPermission | createPermission | updatePermission | deletePermission | rootPermission).run

      whenReady(noPermission, oneMinute) { result =>
        result should be(right = false)
      }
    }
  }

  it should "have only read permission inherited from ACL" in {

    whenReady(grantAclForAUserOnAGroup.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val resourceUri = acl.data.resourceUri
      val uriPattern = UriPattern(acl.data.resourceUri.value)

      whenReady(service.grant(subjectId, resourceUri, readPermission).run, oneMinute) { granted =>

        // it should have no permission
        val noPermission =
          service.hasPermission(subjectId, uriPattern,
            createPermission | updatePermission | deletePermission | rootPermission).run

        whenReady(noPermission, oneMinute) { result =>
          result should be(right = false)
        }

        // it should have read permission on this resource
        val readPermissionR =
          service.hasPermission(subjectId, uriPattern, readPermission).run

        whenReady(readPermissionR, oneMinute) { result =>
          result should be(right = true)
        }
      }
    }
  }

  it should "have only read permission inherited from role" in {

    whenReady(grantAclForAUserOnAGroup.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val resourceUri = acl.data.resourceUri
      val uriPattern = UriPattern(resourceUri.value)

      val addRole =
        roleService.createRole(RoleData(Code("test-role"), readPermission)).flatMap {
          role =>
            service.grant(subjectId, resourceUri, role.id)
        }.run

      whenReady(addRole, oneMinute) { granted =>
        // it should have no permission from:
        val noPermission =
          service.hasPermission(subjectId, uriPattern,
            createPermission | updatePermission | deletePermission | rootPermission).run

        whenReady(noPermission, oneMinute) { result =>
          result should be(right = false)
        }

        // it should have read permission
        val readPermissionR =
          service.hasPermission(subjectId, uriPattern, readPermission).run

        whenReady(readPermissionR, oneMinute) { result =>
          result should be(right = true)
        }
      }
    }
  }

  it should "revoke all subject permissions on resource, given ACL id" in {
    whenReady(grantAclForAUserOnAGroup.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val resourceUri = acl.data.resourceUri
      val uriPattern = UriPattern(acl.data.resourceUri.value)

      val grant =
        for {
          r <- service.grant(subjectId, resourceUri, readPermission)
          u <- service.grant(subjectId, resourceUri, updatePermission)
          d <- service.grant(subjectId, resourceUri, deletePermission)

          hrb <- service.hasPermission(subjectId, uriPattern, readPermission)
          hub <- service.hasPermission(subjectId, uriPattern, updatePermission)
          hdb <- service.hasPermission(subjectId, uriPattern, deletePermission)

          _ <- service.revokeAllPermissions(subjectId)

          hra <- service.hasPermission(subjectId, uriPattern, readPermission)
          hua <- service.hasPermission(subjectId, uriPattern, updatePermission)
          hda <- service.hasPermission(subjectId, uriPattern, deletePermission)

        } yield r && u && d && hrb && hub && hdb && !hra && !hua && !hda

      whenReady(grant.run, oneMinute) { granted =>
        // it should have no permission on this resource
        granted should be(right = true)
      }
    }
  }

  it should "revoke all subject permissions on resource, given a resource and a subject" in {
    whenReady(grantAclForAUserOnAGroup.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val resourceUri = acl.data.resourceUri
      val uriPattern = UriPattern(acl.data.resourceUri.value)

      val grant =
        for {
        // grant permission on a resource
          r <- service.grant(subjectId, resourceUri, readPermission)
          u <- service.grant(subjectId, resourceUri, updatePermission)
          d <- service.grant(subjectId, resourceUri, deletePermission)

          // grant permission on all  resource children
          _ <- service.grant(AclData(acl.data.subject, acl.data.resourceUri.*, noPermission))
          cr <- service.grant(subjectId, resourceUri.*, readPermission)
          cu <- service.grant(subjectId, resourceUri.*, updatePermission)
          cd <- service.grant(subjectId, resourceUri.*, deletePermission)

          // check if has permission on a resource
          hrb <- service.hasPermission(subjectId, uriPattern, readPermission)
          hub <- service.hasPermission(subjectId, uriPattern, updatePermission)
          hdb <- service.hasPermission(subjectId, uriPattern, deletePermission)

          // check if has permission on a resource  children
          chrb <- service.hasPermission(subjectId, uriPattern.*, readPermission)
          chub <- service.hasPermission(subjectId, uriPattern.*, updatePermission)
          chdb <- service.hasPermission(subjectId, uriPattern.*, deletePermission)

          // revoke all permissions on children
          _ <- service.revokeAllPermissions(subjectId, uriPattern.*)

          // check if has permission on a resource
          hra <- service.hasPermission(subjectId, uriPattern, readPermission)
          hua <- service.hasPermission(subjectId, uriPattern, updatePermission)
          hda <- service.hasPermission(subjectId, uriPattern, deletePermission)

          // check if has permission on a resource  children
          chra <- service.hasPermission(subjectId, uriPattern.*, readPermission)
          chua <- service.hasPermission(subjectId, uriPattern.*, updatePermission)
          chda <- service.hasPermission(subjectId, uriPattern.*, deletePermission)

        } yield r && u && d && cr && cu && cd &&
          hrb && hub && hdb && chrb && chub && chdb &&
          hra && hua && hda && !chra && !chua && !chda

      whenReady(grant.run, oneMinute) { granted =>
        // it should have no permission on this resource
        granted should be(right = true)
      }
    }
  }

  it should "revoke a specific permission given to a subject on resource" in {
    whenReady(grantAclForAUserOnAGroup.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val resourceUri = acl.data.resourceUri
      val uriPattern = UriPattern(acl.data.resourceUri.value)

      val grant =
        for {
        // grant permission on a resource
          r <- service.grant(subjectId, resourceUri, readPermission)
          u <- service.grant(subjectId, resourceUri, updatePermission)
          d <- service.grant(subjectId, resourceUri, deletePermission)

          // check if has permission on a resource
          hrb <- service.hasPermission(subjectId, uriPattern, readPermission)
          hub <- service.hasPermission(subjectId, uriPattern, updatePermission)
          hdb <- service.hasPermission(subjectId, uriPattern, deletePermission)

          // revoke only delete permission
          _ <- service.revokePermission(subjectId, uriPattern, deletePermission)

          // check if has permission on a resource
          hra <- service.hasPermission(subjectId, uriPattern, readPermission)
          hua <- service.hasPermission(subjectId, uriPattern, updatePermission)
          hda <- service.hasPermission(subjectId, uriPattern, deletePermission)

        } yield r && u && d && hrb && hub && hdb && hra && hua && !hda

      whenReady(grant.run, oneMinute) { granted =>
        // it should have no permission on this resource
        granted should be(right = true)
      }
    }
  }

  it should "revoke all permission given to a subject on any resource" in {
    whenReady(grantAclForAUserOnAGroup.run, oneMinute) { acl =>

      val subjectId = acl.data.subject.subjectId
      val resourceUri = acl.data.resourceUri
      val uriPattern = UriPattern(acl.data.resourceUri.value)

      val grant =
        for {
        // grant permission on a resource
          r <- service.grant(subjectId, resourceUri, readPermission)
          u <- service.grant(subjectId, resourceUri, updatePermission)
          d <- service.grant(subjectId, resourceUri, deletePermission)

          // grant permission on all  resource children
          _ <- service.grant(AclData(acl.data.subject, acl.data.resourceUri.*, noPermission))
          cr <- service.grant(subjectId, resourceUri.*, readPermission)
          cu <- service.grant(subjectId, resourceUri.*, updatePermission)
          cd <- service.grant(subjectId, resourceUri.*, deletePermission)

          // check if has permission on a resource
          hrb <- service.hasPermission(subjectId, uriPattern, readPermission)
          hub <- service.hasPermission(subjectId, uriPattern, updatePermission)
          hdb <- service.hasPermission(subjectId, uriPattern, deletePermission)

          // check if has permission on a resource  children
          chrb <- service.hasPermission(subjectId, uriPattern.*, readPermission)
          chub <- service.hasPermission(subjectId, uriPattern.*, updatePermission)
          chdb <- service.hasPermission(subjectId, uriPattern.*, deletePermission)

          // revoke all permissions on children
          _ <- service.revokeAllPermissions(subjectId)

          // check if has permission on a resource
          hra <- service.hasPermission(subjectId, uriPattern, readPermission)
          hua <- service.hasPermission(subjectId, uriPattern, updatePermission)
          hda <- service.hasPermission(subjectId, uriPattern, deletePermission)

          // check if has permission on a resource  children
          chra <- service.hasPermission(subjectId, uriPattern.*, readPermission)
          chua <- service.hasPermission(subjectId, uriPattern.*, updatePermission)
          chda <- service.hasPermission(subjectId, uriPattern.*, deletePermission)

        } yield r && u && d && cr && cu && cd &&
          hrb && hub && hdb && chrb && chub && chdb &&
          !hra && !hua && !hda && !chra && !chua && !chda

      whenReady(grant.run, oneMinute) { granted =>
        // it should have no permission on this resource
        granted should be(right = true)
      }
    }
  }

  private def grantAclForAUserOnAGroup = {
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
        val aclData = AclData(subject, resource.resourceUri, noPermission)
        service.grant(aclData)
    }
  }
}