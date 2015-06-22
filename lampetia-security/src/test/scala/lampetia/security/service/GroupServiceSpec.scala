package lampetia.security.service

import java.util.UUID

import lampetia.model.{Code, Email, PlayJson}
import lampetia.security.model._
import lampetia.security.module.SecurityTestModule._
import lampetia.test.LampetiaFutures
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.Json

/**
 * @author Radwa Osama
 */
class GroupServiceSpec extends FlatSpec with Matchers with ScalaFutures with LampetiaFutures {
  implicit val ec = executionContext

  val service = new GroupService {}
  val userService = new UserService {}

  final val EMPTY = ""

  def groupRef(ownerId: UserId, parentGroupId: Option[GroupId] = None): GroupRef = GroupRef(ownerId, parentGroupId)

  it should "create group" in {
    val u = userService.createUser(profileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val group = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(group, oneMinute) { result =>
        result.id.value shouldNot be(EMPTY)
      }
    }
  }

  it should "create group with parent group" in {
    val u = userService.createUser(profileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val p = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(p, oneMinute) { parent =>
        parent.id.value shouldNot be(EMPTY)

        // Add child group
        def childGroupData =
          GroupData(code = Code(UUID.randomUUID.toString))
        val childGroup = service.createGroup(groupRef(owner.id, Some(parent.id)), childGroupData).run
        whenReady(childGroup, oneMinute) { child =>
          child.id.value shouldNot be(EMPTY)
        }
      }
    }
  }

  it should "find group By group Id" in {
    val u = userService.createUser(profileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val group = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(group, oneMinute) { result =>
        result.id.value shouldNot be(EMPTY)
        val selectedGroup = service.findGroupByGroupId(result.id).run
        whenReady(selectedGroup, oneMinute) { selection =>
          selection shouldNot be(None)
          selection.get.id should be(result.id)
        }
      }
    }
  }

  /*it should "find group By parent group Id" in {
    val u = userService.createUser(profileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val pf = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(pf, oneMinute) { parent =>
        parent.id.value shouldNot be(EMPTY)
        def childGroupData =
          GroupData(Code(UUID.randomUUID.toString))
        val childGroup = service.createGroup(groupRef(owner.id, Some(parent.id)), childGroupData).run
        whenReady(childGroup, oneMinute) { child =>
          child.id.value shouldNot be(EMPTY)

          val selectedChild = service.findGroupByParentGroupId(parent.id).run
          whenReady(selectedChild, oneMinute) { result =>
            result shouldNot be(Nil)
            result.size should be(1)
            result.head.id.value shouldNot be(None)
            result.head.id.value should be(child.id.value)
          }
        }
      }
    }
  }*/

  it should "try to find non-existing group By group Id" in {
    val selectedGroup = service.findGroupByGroupId(GroupId("none")).run
    whenReady(selectedGroup, oneMinute) { selection =>
      selection should be(None)
    }
  }

  it should "add group member" in {
    val u = userService.createUser(profileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val g = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(g, oneMinute) { group =>
        group.id.value shouldNot be(EMPTY)
        // add new user
        val u = userService.createUser(profileData).run

        whenReady(u, oneMinute) { user =>

          user.id.value shouldNot be(EMPTY)

          val um = service.addMember(group.id, user.id).run

          whenReady(um, oneMinute) { result =>
            result should be(1)
          }

        }
      }
    }
  }

  it should "remove group member" in {
    val u = userService.createUser(profileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val g = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(g, oneMinute) { group =>
        group.id.value shouldNot be(EMPTY)
        // add new user
        val u = userService.createUser(profileData).run

        whenReady(u, oneMinute) { user =>
          user.id.value shouldNot be(EMPTY)
          val am = service.addMember(group.id, user.id).run
          whenReady(am, oneMinute) { amr =>
            amr should be(1)
            val rm = service.removeMember(group.id, user.id).run
            whenReady(rm, oneMinute) { rmr =>
              rmr should be(1)
            }
          }
        }
      }
    }
  }

  it should "find all" in {
    val groups = service.findAll(10).run
    whenReady(groups, oneMinute) { result =>
      result.size should be >= 0
    }
  }

  it should "remove group" in {
    val u = userService.createUser(profileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val g = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(g, oneMinute) { group =>
        group.id.value shouldNot be(EMPTY)
        val rg = service.removeGroup(group.id).run
        whenReady(rg, oneMinute) { result =>
          result should be(1)
        }
      }
    }
  }

  it should "find child groups by parent" in {

    def groupData = GroupData(Code(UUID.randomUUID.toString))


    val p = for {
      u <- userService.createUser(profileData)
      parent <- service.createGroup(GroupRef(u.id, None), groupData)
      a1 <- service.createGroup(GroupRef(u.id, Some(parent.id)), groupData)
      a11 <- service.createGroup(GroupRef(u.id, Some(a1.id)), groupData)
      a12 <- service.createGroup(GroupRef(u.id, Some(a1.id)), groupData)
      a2 <- service.createGroup(GroupRef(u.id, Some(parent.id)), groupData)
      a21 <- service.createGroup(GroupRef(u.id, Some(a2.id)), groupData)
      a22 <- service.createGroup(GroupRef(u.id, Some(a2.id)), groupData)
      a3 <- service.createGroup(GroupRef(u.id, Some(parent.id)), groupData)
      children <- service.findChildGroupsByParentGroupId(parent.id)
      _ <- service.removeGroup(a11.id)
      _ <- service.removeGroup(a12.id)
      _ <- service.removeGroup(a1.id)
      _ <- service.removeGroup(a21.id)
      _ <- service.removeGroup(a22.id)
      _ <- service.removeGroup(a2.id)
      _ <- service.removeGroup(a3.id)
      _ <- service.removeGroup(parent.id)
    } yield children

    val f = p.transactionally.run
    whenReady(f, oneMinute) { groups =>
      //groups.foreach(println)
      //groups shouldNot be(Seq.empty[Group])
      groups.length should be(8)
    }
  }

  def profileData = {
    val email = s"${UUID.randomUUID.toString}@test.org"
    ProfileData(
      UsernamePasswordProvider,
      ProviderUserId(EMPTY),
      ProviderResponse(PlayJson(Json.parse("[]"))),
      Email(email),
      Some(Password("unsafe")),
      AccountDetails(PlayJson(Json.parse("[]"))))
  }

}
