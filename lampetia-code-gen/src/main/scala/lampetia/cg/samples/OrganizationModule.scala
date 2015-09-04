package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module

/**
 * @author Hossam Karim   --  provisioning Fathi Al-Ghaiati
 */

object OrganizationModule extends  App {

  //Table: OrganizationType
  val LanguageModel =
    enum("Language")("English", "Arabic", "French", "Germany")
      .withDiscriminator("language".string) <+ commonFeatures

  val TextInLanguageModel =
    composite("TextInLanguage") (
      "language" of LanguageModel,
      "text".string
    )  <+ commonFeatures


  val OrganizationTypeNameInfoModel =
    composite("OrganizationTypeNameInfo")(
      "name" listOf TextInLanguageModel<< (jsonbArrayComposite in Sql)
    ) <+ commonFeatures << (jsonbComposite in Sql)

  val OrganizationTypeModel =
    entity("OrganizationType")(
      "code".string,
      ("nameInfo" of OrganizationTypeNameInfoModel )<< (jsonbComposite in Sql)
    ).withResourceType("lampetia.model.organization:1.0") <+ commonFeatures <<
      (named("organization_type") in Sql)

  //Table: Organization

  val OrganizationPackageModel =
    enum("OrganizationPackage")("Public", "Private", "Enterprise")
      .withDiscriminator("packageCode".string) <+ commonFeatures

  val OrganizationStateModel =
    enum("OrganizationState")("OrganizationCreating", "OrganizationEditing", "OrganizationActive", "OrganizationSuspended", "OrganizationArchived", "OrganizationDeleted")
      .withDiscriminator("state".string) <+ commonFeatures

  val CountryModel = value("Country")("code".string) <+ commonFeatures  //to implement as service link to other site
  val CityModel = value("City")("code".string) <+ commonFeatures  //to implement as service link to other site
  val OrganizationAddressModel = composite("OrganizationAddress")("mailAddress".string, "gisAddress".string) <+ commonFeatures
  val UrlModel = value("Url")("address".string) <+ commonFeatures
/*
  val OrganizationContactModel =
    composite("OrganizationContact")(
      "contactUserId".userId,
      "contactName".name,
      "contactEmail".string
    ) <+ commonFeatures
*/

  val OrganizationInfoModel =
    composite("OrganizationInfo")(
      "country" of CountryModel,
      "city" of CityModel,
      "cityName".string,   // denormalized city name or entered by user as other
      "address" of OrganizationAddressModel,
      "websiteUrl" of UrlModel,
      "logoUrl" of UrlModel,
      /*"contact" of OrganizationContactModel,*/
      "contactUserId".userId,
      "contactName".name,
      "contactEmail".email
    ) <+ commonFeatures << (jsonbComposite in Sql)

  val OrganizationModel =
    entity("Organization")(
      "name".name,
      "typeId" ref OrganizationTypeModel.id,
      "packageCode" of OrganizationPackageModel,
      "state" of OrganizationStateModel,
      ("organizationInfo" of OrganizationInfoModel) << (jsonbComposite in Sql)
    ).withResourceType("lampetia.model.organization:1.0") <+ commonFeatures <<
  (named("organization") in Sql)

  lazy val module = Module("Org", "lampetia.organization", "org")
  val models = Seq(
    LanguageModel,
    TextInLanguageModel,
    OrganizationTypeNameInfoModel,
    OrganizationTypeModel.id.tpe,
    OrganizationTypeModel,
    OrganizationPackageModel,
    OrganizationStateModel,
    CountryModel,
    CityModel,
    OrganizationAddressModel,
    UrlModel,
    OrganizationInfoModel,
    OrganizationModel.id.tpe,
    OrganizationModel)

  CodeGenerator.serviceGenerator(module, models).generate()

}
