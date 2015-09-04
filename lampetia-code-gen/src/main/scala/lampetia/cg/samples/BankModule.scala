package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module

/**
 * @author Hossam Karim
 */


object BankModule extends App {

  val base = "com.bank"
  val module = Module("Bank", base, "bank")

  val CustomerTypeModel =
    enum("CustomerType")("Individual".ecase, "Organization".ecase).withDiscriminator("customerType".string) <+ commonFeatures

  val CountryModel =
    entity("Country")("name".name) <+ commonFeatures

  val CityModel =
    entity("City")("name".name, "country" ref CountryModel.id).withResourceType(s"$base.city:1.0") <+ commonFeatures

  val AddressModel = value("Address")("value".string) <+ commonFeatures

  val BranchModel = composite("Branch")("branchName".name, "branchAddress" of AddressModel) <+ commonFeatures

  val BuildingModel =
    composite("Building")("buildingName".name, "buildingManager" of SubjectId) << (jsonbComposite in Sql)

  val EmployeeModel =
    entity("Employee")("name".name) <+ commonFeatures

  val BankModel =
    entity("Bank")(
      "address" of AddressModel,
      "city" ref CityModel.id,
      "manager" ref EmployeeModel.id,
      "customerType" of CustomerTypeModel,
      ("branch" of BranchModel) << (flatten() in Sql),
      ("building" of BuildingModel) << (jsonbComposite in Sql),
      "comments".jsond).withResourceType(s"$base.bank:1.0") <+ commonFeatures



  val models = Seq(
    CountryModel.id.tpe,
    CountryModel,
    CityModel.id.tpe,
    CityModel,
    EmployeeModel.id.tpe,
    EmployeeModel,
    CustomerTypeModel,
    BankModel.id.tpe,
    AddressModel,
    BranchModel,
    BuildingModel,
    BankModel)

  CodeGenerator.serviceGenerator(module, models).generate()

}
