package lampetia.cg.samples

/**
 * @author Hossam Karim
 */

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.{Secure, Entity, Module}

object CompanyModule extends App {

  val base = "com.company"

  val module = Module("Company", base, "company", options = Seq(Secure))

  val employeePartialModel = entity("Employee")()

  val departmentModel: Entity =
    entity("Department")(
      "name".name,
      "manager" ref employeePartialModel.id)

  val agreement =
    composite("Agreement")("percentage".int, "vesting".string)

  val employeeStockOptions =
    composite("StockOptions")(
      ("agreement" of agreement) << (flatten() in Sql),
      "bank".name,
      "degree".int,
      "since".dateTime)

  val employeeModel: Entity =
    extend(employeePartialModel)(
      "name".name,
      "salary".int,
      "department" optionalRef departmentModel.id,
      ("options" of employeeStockOptions) << (flatten() in Sql))



  val models = Seq(
    agreement,
    employeeStockOptions,
    employeeModel.id.tpe,
    employeeModel,
    departmentModel.id.tpe,
    departmentModel)

  CodeGenerator.modelGenerator(module, models).generate()
  //CodeGenerator.scriptGenerator(module, models).generate(formatScalaSource = false)
  //CodeGenerator.nodeGenerator(module, models).generate(formatScalaSource = false)
  //CodeGenerator.deployGenerator(module, models).generate(formatScalaSource = false)
  //CodeGenerator.utilGenerator(module, models).generate(formatScalaSource = false)

}
