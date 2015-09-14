package lampetia.security.format

import java.util.UUID

import lampetia.model.{UserId, Code, Email}
import lampetia.security.model._
import org.scalatest.{Matchers, FlatSpec}
import play.api.libs.json.{JsResult, Json}

/**
 * @author Hossam Karim
 */

class SecurityJsonFormatSpec extends FlatSpec with Matchers {

  def random = UUID.randomUUID.toString

  val format = new SecurityJsonFormat {}

  import format._

  it should "perform User roundtrip json formatting" in {
    val user = User(UserId(random), AccountActive)
    val json = Json.stringify(Json.toJson(user))
    //println(json)
    val roundtrip: JsResult[User] = Json.fromJson[User](Json.parse(json))
    roundtrip.asOpt shouldEqual Some(user)
  }

  it should "perform Profile roundtrip json formatting" in {
    val profileData =
      ProfileData(
        UsernamePasswordProvider,
        ProviderUserId(""),
        ProviderResponse(Json.parse("[]")),
        Email(s"${UUID.randomUUID.toString}@test.org"),
        Some(Password("unsafe")),
        AccountDetails(Json.parse("[]")))
    val profile =
      Profile(ProfileId(random), ProfileRef(UserId(random)), profileData)

    val json = Json.stringify(Json.toJson(profile))
    //println(json)
    val roundtrip: JsResult[Profile] = Json.fromJson[Profile](Json.parse(json))
    roundtrip.asOpt shouldEqual Some(profile)

  }

  it should "perform Group roundtrip json formatting" in {
    val group = Group(GroupId(random), GroupRef(UserId(random), None), GroupData(Code(random)))
    val json = Json.stringify(Json.toJson(group))
    //println(json)
    val roundtrip: JsResult[Group] = Json.fromJson[Group](Json.parse(json))
    roundtrip.asOpt shouldEqual Some(group)
  }

}
