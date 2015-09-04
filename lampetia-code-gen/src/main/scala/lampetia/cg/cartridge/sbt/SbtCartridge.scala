package lampetia.cg.cartridge.sbt

import lampetia.extensions.Strings
import Strings._
import lampetia.cg.model._
import lampetia.metamodel.{DependsOn, Module, FeatureScope, Model}
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

/**
 * @author Hossam Karim
 */
case class SbtProject(
   name: String,
   subProjects: Seq[SbtProject] = Nil,
   dependencies: Seq[SbtProject] = Nil,
   libraries: Seq[String] = Nil,
   plugins: Seq[String] = Nil,
   extraModels: Map[String, Any] = Map.empty[String, Any],
   customTemplate: Option[String] = None)

trait SbtCartridge extends Cartridge[SbtProject] {

  def configRoot = "cg.template.sbt"


  trait SbtFileGenerationTask extends SingleOutputFileGenerationTask {
    def project: SbtProject

    def key: String

    def outputDir: CDir

    def outputFile: CFile

    def template = CTemplate(config.as[String](s"$configRoot.$key"))

    def runSingle(models: Seq[Model]) =
      whenAvailable(key) {
        FileGeneration(template, outputFile, outputDir, Map("project" -> project) ++ project.extraModels)
      }
  }

  case class CommonSbtFileGenerationTask(project: SbtProject, key: String, outputFile: CFile, outputDir: CDir) extends SbtFileGenerationTask

  def pluginsTask(project: SbtProject) =
    CommonSbtFileGenerationTask(project, "plugins", CFile("plugins.sbt"), CDir("project"))

  def commonTask(project: SbtProject) =
    CommonSbtFileGenerationTask(project, "common", CFile("Common.scala"), CDir("project"))

  def rootBuildTask(project: SbtProject) =
    CommonSbtFileGenerationTask(project, "root-project-build", CFile("Build.scala"), CDir("project"))

  def subProjectBuildTask(project: SbtProject) = project.customTemplate match {
    case Some(t) =>
      CommonSbtFileGenerationTask(project, t, CFile("build.sbt"), CDir(s"${project.name.lispCase}"))
    case None =>
      CommonSbtFileGenerationTask(project, "sub-project-build", CFile("build.sbt"), CDir(s"${project.name.lispCase}"))
  }

  def buildPropertiesTask(project: SbtProject) =
    CommonSbtFileGenerationTask(project, "build-properties", CFile("build.properties"), CDir("project"))
}

class DefaultSbtCartridge(val config: Config) extends SbtCartridge {

  def tasksFor(project: SbtProject): Seq[Task] = {
    val plugins = pluginsTask(project)
    val common = commonTask(project)
    val root = rootBuildTask(project)
    val buildProperties = buildPropertiesTask(project)
    val subProjects = project.subProjects.map(subProjectBuildTask)
    //subProjects ++ Seq(plugins, common, root, buildProperties)
    subProjects ++ Seq(root)
  }

}
