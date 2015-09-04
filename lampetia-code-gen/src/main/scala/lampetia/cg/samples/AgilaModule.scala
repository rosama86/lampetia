package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.{StringLiteral, Module}

/**
 * @author Hossam Karim
 */
object AgilaModule extends App {

  //val CommentModel = ???

  val OrganizationModel =
    entity("Organization")("name".name) <+ commonFeatures

  val ProjectModel =
    entity("Project")(
      "name".name,
      "organization" ref OrganizationModel.id
    ) <+ commonFeatures

  val ReleaseModel =
    entity("Release")(
      "name".name,
      "project" ref ProjectModel.id
    ) <+ commonFeatures

  val SprintModel =
    entity("Sprint")(
      "release" ref ReleaseModel.id,
      "name".name,
      "number".int
    ) <+ commonFeatures

  val StoryModel =
    entity("Story")(
      "sprint" ref SprintModel.id,
      "title".title,
      "body".string
    ) <+ commonFeatures

  val ScenarioContextModel =
    composite("Context")("values" listOf StringLiteral) <+ commonFeatures << (jsonbComposite in Sql)
  val ScenrioOutcomeModel =
    composite("Outcome")("values" listOf StringLiteral) <+ commonFeatures << (jsonbComposite in Sql)

  val ScenarioModel =
    entity("Scenario")(
      "story" ref StoryModel.id,
      "title".title,
      ("context" of ScenarioContextModel) << (jsonbComposite in Sql),
      "event" optionOf StringLiteral,
      ("outcome" of ScenrioOutcomeModel) << (jsonbComposite in Sql)
    ) <+ commonFeatures

  val module = Module("Agila", "lampetia.agila", "agila")

  val models =
    Seq(
      OrganizationModel.id.tpe, OrganizationModel,
      ProjectModel.id.tpe, ProjectModel,
      ReleaseModel.id.tpe, ReleaseModel,
      SprintModel.id.tpe, SprintModel,
      StoryModel.id.tpe, StoryModel,
      ScenarioContextModel, ScenrioOutcomeModel, ScenarioModel.id.tpe, ScenarioModel
    )

  CodeGenerator.serviceGenerator(module, models).generate()

}
