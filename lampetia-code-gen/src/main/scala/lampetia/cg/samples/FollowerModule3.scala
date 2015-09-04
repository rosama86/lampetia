package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module

/**
 * @author Hossam Karim
 */
object FollowerModule3 extends App {


  def indexer(sqlName: String): Boolean =
    sqlName == "user_id" || sqlName == "resource_id"

  val FollowerModel =
    entity("Follower")(
      "userId".userId,
      ("follower".resource) << (flatten("id" -> "resourceId") in Sql)
    ) <+ commonFeatures << (uniqueIndex(indexer) in Sql)

  val module =
    Module("Follower", "lampetia.follower", "lampetia")

  val models = Seq(FollowerModel.id.tpe, FollowerModel)
  CodeGenerator.serviceGenerator(module, models).generate()




}
