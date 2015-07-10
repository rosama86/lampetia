package lampetia.security.service

import lampetia.model.Code
import lampetia.security.model.RoleData
import lampetia.security.module.SecurityTestModule._
import lampetia.test.LampetiaFutures
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

/**
 * @author Radwa Osama
 */
class RoleServiceSpecs extends FlatSpec with Matchers with ScalaFutures with LampetiaFutures {
  implicit val ec = executionContext

  val service = new RoleService {}

  it should "create a new role" in {

    val roleData = RoleData(code = Code("TEST_ADMIN_PERM"), permission = adminPermission)
    val rIO = service.createRole(roleData).run

    whenReady(rIO, oneMinute) { role =>
      role.id.value shouldNot be("")
    }
  }

  it should "find a role by Id" in {

    val roleData = RoleData(code = Code("TEST_ADMIN_PERM"), permission = adminPermission)
    val rIO = service.createRole(roleData).run

    whenReady(rIO, oneMinute) { role =>
      role.id.value shouldNot be("")

      val selectIO = service.findRoleByRoleId(role.id).run

      whenReady(selectIO, oneMinute) { sRole =>

        sRole shouldNot be(None)

        sRole.get.id.value shouldNot be("")
        sRole.get.id.value should be(role.id.value)
        sRole.get.data.permission.code should be(role.data.permission.code)
        sRole.get.data.code.value should be(role.data.code.value)
      }

    }
  }

  it should "find all" in {

    val roleData = RoleData(code = Code("TEST_ADMIN_PERM"), permission = adminPermission)
    val rIO = service.createRole(roleData).run

    whenReady(rIO, oneMinute) { role =>
      role.id.value shouldNot be("")

      val selectIO = service.findAll(100).run

      whenReady(selectIO, oneMinute) { sRole =>

        sRole shouldNot be(Nil)
        sRole.size should be > 0
      }

    }
  }

  it should "remove role by Id" in {

    val roleData = RoleData(code = Code("TEST_ADMIN_PERM"), permission = adminPermission)
    val rIO = service.createRole(roleData).run

    whenReady(rIO, oneMinute) { role =>
      role.id.value shouldNot be("")

      val countIO = service.removeRole(role.id).run

      whenReady(countIO, oneMinute) { count =>
        count should be(1)
      }

    }
  }

}
