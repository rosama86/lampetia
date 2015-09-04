package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module

/**
 * @author Hossam Karim
 */

object AboutMeModule extends  App {

  val CountryModel = value("Country")("code".string) <+ commonFeatures

  val IndustryModel = value("Industry")("code".string) <+ commonFeatures

  val PhoneModel = composite("Phone")("home".string, "work".string) <+ commonFeatures

  val AddressModel = composite("Address")("primaryAddress".string, "workAddress".string) <+ commonFeatures

  val UrlModel = value("Url")("address".string) <+ commonFeatures

  val SkillModel = composite("Skill")("name".name, "description".string) <+ commonFeatures

  val ExperienceModel =
    composite("Experience")(
      "organizationName".name,
      "organizationUrl" of UrlModel,
      "startDate".dateTime,
      "endDate".dateTime,
      "title".title,
      "roleDescription".string,
      "organizationSystemResource".resource
    ) <+ commonFeatures

  val EducationalDegreeModel = value("EducationalDegree")("code".string) <+ commonFeatures

  val EducationModel =
    composite("Education")(
      "institute".name,
      "startDate".dateTime,
      "endDate".dateTime,
      "degree" of EducationalDegreeModel,
      "instituteSystemResource".resource
    ) <+ commonFeatures

  val AboutMeStateModel =
    enum("AboutMeState")("Inserted".ecase, "Editing".ecase, "Active".ecase, "Deleted".ecase)
      .withDiscriminator("state".string) <+ commonFeatures

  val MaritalStatusModel =
    enum("MaritalStatus")("Single".ecase, "Married".ecase).withDiscriminator("state".string) <+ commonFeatures

  val AboutMeInfoModel =
    composite("AboutMeInfo")(
      "photoUrl" of UrlModel,
      "birthdate".dateTime,
      "maritalStatus" optionOf MaritalStatusModel,
      "phone" of PhoneModel,
      "address" of AddressModel,
      "title".title,
      "country" of CountryModel,
      "industry" of IndustryModel,
      "publicProfileUrl" of UrlModel,
      "skills" listOf SkillModel,
      "links" listOf UrlModel,
      "experience" listOf ExperienceModel,
      "education" listOf EducationModel
    ) <+ commonFeatures << (jsonbComposite in Sql)

  val AboutMeModel =
    entity("AboutMe")(
      "state" of AboutMeStateModel,
      "otherUrl" optionOf UrlModel,
      ("aboutMeInfo" of AboutMeInfoModel) << (jsonbComposite in Sql),
      ("aboutMeEditingInfo" of AboutMeInfoModel) << (jsonbComposite in Sql)
    ).withResourceType("lampetia.model.about-me:1.0") <+ commonFeatures

  lazy val module = Module("AboutMe", "lampetia.aboutme", "lampetia")
  val models = Seq(
    CountryModel,
    IndustryModel,
    PhoneModel,
    AddressModel,
    UrlModel,
    SkillModel,
    ExperienceModel,
    EducationalDegreeModel,
    EducationModel,
    AboutMeStateModel,
    MaritalStatusModel,
    AboutMeInfoModel,
    AboutMeModel.id.tpe,
    AboutMeModel)

  CodeGenerator.serviceGenerator(module, models).generate()


}
