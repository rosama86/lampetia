package lampetia.cg.samples

/**
 * @author Hossam Karim
 */

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.{Secure, Module}

object AbdalDemo extends App {


  val base = "me.abdal"

  val module = Module("AbdalDemo", base, "abdal", options = Seq(Secure))

  val Company = entity("Company")("data".jsonb) <+ (commonFeatures)

  val models = Seq(Company.id.tpe, Company)

  CodeGenerator.serviceGenerator(module, models).generate()
  //CodeGenerator.scriptGenerator(module, models).generate(formatScalaSource = false)
  //CodeGenerator.nodeGenerator(module, models).generate(formatScalaSource = false)
  //CodeGenerator.deployGenerator(module, models).generate(formatScalaSource = false)
  //CodeGenerator.utilGenerator(module, models).generate(formatScalaSource = false)


}
