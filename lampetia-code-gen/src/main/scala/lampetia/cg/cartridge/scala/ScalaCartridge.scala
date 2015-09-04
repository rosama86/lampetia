package lampetia.cg.cartridge.scala

import lampetia.cg.model._
import lampetia.extensions.Strings
import lampetia.metamodel.Dsl._
import lampetia.metamodel._
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import Strings._

/**
 * @author Hossam Karim
 */

trait ScalaCartridge extends Cartridge[Module] { self =>

  def baseDir(module: Module): String
  def scalaDir(module: Module) = s"${baseDir(module)}/src/generated/scala/${module.basePackage.packageToDir}"
  def scalaTestDir(module: Module) = s"${baseDir(module)}/src/test/scala/${module.basePackage.packageToDir}"
  def resourceDir(module: Module) = s"${baseDir(module)}/src/generated/resources"
  def resourceTestDir(module: Module) = s"${baseDir(module)}/src/test/resources"
  def scope: FeatureScope

  trait ScalaFileGenerationTask extends SingleOutputFileGenerationTask {
    def config: Config = self.config
    def configRoot: String = self.configRoot
    def module: Module
    def key: String
    def outputDir: CDir
    def outputFile: CFile
    def taskModels(models: Seq[Model]): Map[String, Any] = Map("module" -> module, "metamodels" -> models)
    def template = CTemplate(config.as[String](s"$configRoot.$key"))
    def runSingle(models: Seq[Model]) =
      whenAvailable(key) {
        FileGeneration(template, outputFile, outputDir, taskModels(models))
      }
  }

  case class CommonScalaFileGenerationTask(
    module: Module,
    key: String,
    outputFile: CFile,
    outputDir: CDir,
    requiredPackages: Module => Seq[String] = _ => Nil,
    extraModels: Map[String, Any] = Map.empty[String, Any]) extends ScalaFileGenerationTask {
    val fullKey = s"$configRoot.$key"

    // Imports explicitly attached to the module
    def imports: Seq[String] =
      Template.keywordsIn(module).collect {
        case Imports(t, pkgs) if t == fullKey => pkgs
      }.flatten

    // Imports calculated by prefixing the dependecy module package to the required sub-package by each task
    def dependencies: Seq[String] =
      scope.keywordsIn(module).collect {
        case DependsOn(sub) =>
          sub.parent
      }.flatMap(parent => requiredPackages(parent)).toSet.toSeq

    override def taskModels(models: Seq[Model]): Map[String, Any] =
      super.taskModels(models) ++ extraModels ++ Map("imports" -> (imports ++ dependencies))
  }

  def sbtDependencies(module: Module, scope: FeatureScope): Seq[String] =
    scope.keywordsIn(module).collect {
      case DependsOn(Submodule(parent, sub)) =>
        val organization =
          parent.organization(sub).getOrElse(config.as[String](s"cg.template.sbt.default-organization")).qoute
        val id = parent.artifactId(sub).qoute
        val version =
          parent.version(sub).getOrElse(config.as[String](s"cg.template.sbt.default-version")).qoute
        val compileAndTest = "compile->compile;test->test"
        s"$organization %% $id % $version % ${compileAndTest.qoute}"
    }

}
