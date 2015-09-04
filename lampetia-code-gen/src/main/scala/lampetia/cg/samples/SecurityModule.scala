package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module

/**
 * @author Hossam Karim
 */

object SecurityModule extends App {

  val UserIdModel = idToValue(User.id.tpe)

  val GroupModel =
    entity("Group")(
      "groupOwner".userId, "name".name, "users" listOf UserIdModel
    ).withResourceType("lampetia.group:1.0") <+ commonFeatures

  lazy val module = Module("Security", "lampetia.security", "lampetia")
  val models = Seq(GroupModel.id.tpe, GroupModel)

  CodeGenerator.serviceGenerator(module, models).generate()

}
