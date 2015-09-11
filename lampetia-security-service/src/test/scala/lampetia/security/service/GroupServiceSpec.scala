package lampetia.security.service

import java.util.UUID

import lampetia.model._
import lampetia.security.model._
import lampetia.security.module.SecurityTestModule._
import lampetia.test.LampetiaFutures
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

/**
 * @author Radwa Osama
 */
class GroupServiceSpec extends FlatSpec with Matchers with ScalaFutures with LampetiaFutures with CommonServiceSpec {

  import dialect._
  import sql._

  implicit val ec = configuration.concurrent.executionContext

  val service = new GroupService {}
  val userService = new UserService {}

  it should "create group" in {
    val u = userService.createUser(testProfileData).run
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
    val u = userService.createUser(testProfileData).run
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
    val u = userService.createUser(testProfileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val group = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(group, oneMinute) { result =>
        result.id.value shouldNot be(EMPTY)
        val selectedGroup = service.findOne(result.id).run
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
    val selectedGroup = service.findOne(GroupId("none")).run
    whenReady(selectedGroup, oneMinute) { selection =>
      selection should be(None)
    }
  }

  it should "add group member" in {
    val u = userService.createUser(testProfileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val g = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(g, oneMinute) { group =>
        group.id.value shouldNot be(EMPTY)
        // add new user
        val u = userService.createUser(testProfileData).run

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
    val u = userService.createUser(testProfileData).run
    whenReady(u, oneMinute) { owner =>
      def groupData =
        GroupData(code = Code(UUID.randomUUID.toString))
      val g = service.createGroup(groupRef(owner.id), groupData).run
      whenReady(g, oneMinute) { group =>
        group.id.value shouldNot be(EMPTY)
        // add new user
        val u = userService.createUser(testProfileData).run

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

  it should "remove group" in {
    val u = userService.createUser(testProfileData).run
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
      u <- userService.createUser(testProfileData)
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
      groups.length should be(8)
    }
  }


  it should "find all members inside the group and its children by group" in {

    def groupData = GroupData(Code(UUID.randomUUID.toString))


    val p = for {
      u <- userService.createUser(testProfileData)
      parent <- service.createGroup(GroupRef(u.id, None), groupData)
      _ <- service.addMember(parent.id, u.id)

      a1 <- service.createGroup(GroupRef(u.id, Some(parent.id)), groupData)
      a1u1 <- userService.createUser(testProfileData)
      _ <- service.addMember(a1.id, a1u1.id)
      a1u2 <- userService.createUser(testProfileData)
      _ <- service.addMember(a1.id, a1u2.id)

      a11 <- service.createGroup(GroupRef(u.id, Some(a1.id)), groupData)
      a11u1 <- userService.createUser(testProfileData)
      _ <- service.addMember(a11.id, a11u1.id)
      a11u2 <- userService.createUser(testProfileData)
      _ <- service.addMember(a11.id, a11u2.id)

      a12 <- service.createGroup(GroupRef(u.id, Some(a1.id)), groupData)
      a12u1 <- userService.createUser(testProfileData)
      _ <- service.addMember(a12.id, a12u1.id)
      a12u2 <- userService.createUser(testProfileData)
      _ <- service.addMember(a12.id, a12u2.id)

      a2 <- service.createGroup(GroupRef(u.id, Some(parent.id)), groupData)
      a2u1 <- userService.createUser(testProfileData)
      _ <- service.addMember(a2.id, a2u1.id)
      a2u2 <- userService.createUser(testProfileData)
      _ <- service.addMember(a2.id, a2u2.id)

      a21 <- service.createGroup(GroupRef(u.id, Some(a2.id)), groupData)
      a21u1 <- userService.createUser(testProfileData)
      _ <- service.addMember(a21.id, a21u1.id)
      a21u2 <- userService.createUser(testProfileData)
      _ <- service.addMember(a21.id, a21u2.id)

      a22 <- service.createGroup(GroupRef(u.id, Some(a2.id)), groupData)
      a22u1 <- userService.createUser(testProfileData)
      _ <- service.addMember(a22.id, a22u1.id)
      a22u2 <- userService.createUser(testProfileData)
      _ <- service.addMember(a22.id, a22u2.id)

      a3 <- service.createGroup(GroupRef(u.id, Some(parent.id)), groupData)
      a3u1 <- userService.createUser(testProfileData)
      _ <- service.addMember(a3.id, a3u1.id)
      a3u2 <- userService.createUser(testProfileData)
      _ <- service.addMember(a3.id, a3u2.id)

      childrenMembers <- service.findMembers(parent.id)
      _ <- service.removeMember(a11.id, a11u1.id)
      _ <- service.removeMember(a11.id, a11u2.id)
      _ <- service.removeGroup(a11.id)
      _ <- service.removeMember(a12.id, a12u1.id)
      _ <- service.removeMember(a12.id, a12u2.id)
      _ <- service.removeGroup(a12.id)
      _ <- service.removeMember(a1.id, a1u1.id)
      _ <- service.removeMember(a1.id, a1u2.id)
      _ <- service.removeGroup(a1.id)
      _ <- service.removeMember(a21.id, a21u1.id)
      _ <- service.removeMember(a21.id, a21u2.id)
      _ <- service.removeGroup(a21.id)
      _ <- service.removeMember(a22.id, a22u1.id)
      _ <- service.removeMember(a22.id, a22u2.id)
      _ <- service.removeGroup(a22.id)
      _ <- service.removeMember(a2.id, a2u1.id)
      _ <- service.removeMember(a2.id, a2u2.id)
      _ <- service.removeGroup(a2.id)
      _ <- service.removeMember(a3.id, a3u1.id)
      _ <- service.removeMember(a3.id, a3u2.id)
      _ <- service.removeGroup(a3.id)
      _ <- service.removeMember(parent.id, u.id)
      _ <- service.removeGroup(parent.id)
    } yield childrenMembers

    val f = p.transactionally.run
    whenReady(f, oneMinute) { members =>
      members.length should be(15)
    }
  }
}
