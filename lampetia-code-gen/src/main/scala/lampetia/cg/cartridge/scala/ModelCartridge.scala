package lampetia.cg.cartridge.scala

import lampetia.cg.cartridge.sbt.{DefaultSbtCartridge, SbtProject}
import lampetia.cg.model._
import lampetia.extensions.Strings
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import Strings._

/**
 * @author Hossam Karim
 */

trait ModelCartridge extends ScalaCartridge {

  def configRoot = "cg.template.model"
  def baseDir(module: Module) = s"${module.name.lispCase}-model"
  def scope = ModelScope

  def sbtProject(module: Module, dependencies: SbtProject*): SbtProject = {
    val libs = config.as[Seq[String]](s"$configRoot.sbt-libraries") ++ sbtDependencies(module, ModelScope)
    SbtProject(
      name = s"${module.name}Model",
      dependencies = dependencies,
      libraries = libs,
      customTemplate = Some("model-build")
    )
  }

  def sbtTasksFor(module: Module): Seq[FileGenerationTask] = {
    val project = sbtProject(module)
    Seq(new DefaultSbtCartridge(config).subProjectBuildTask(project))
  }

  def packageObject(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "models",
      CFile("models.scala"),
      CDir(s"${scalaDir(module)}/model"),
      m => Seq(s"${m.basePackage}.model._"))

  def jsonFormat(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "json-format",
      CFile(s"${module.name}JsonFormat.scala"),
      CDir(s"${scalaDir(module)}/format/json"),
      m => Seq(
        s"${m.basePackage}.model._",
        s"${m.basePackage}.format.json.${m.name}JsonFormat._"))

  def messagePackFormat(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "messagepack-format",
      CFile(s"${module.name}MessagePackFormat.scala"),
      CDir(s"${scalaDir(module)}/format/mp"),
      m => Seq(
        s"${m.basePackage}.model._",
        s"${m.basePackage}.format.mp.${m.name}MessagePackFormat._"))

  def jsonFormatSpec(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "json-format-spec",
      CFile(s"${module.name}JsonFormatSpec.scala"),
      CDir(s"${scalaTestDir(module)}/format/json"),
      m => Seq(s"${m.basePackage}.model._"))

  def messagePackFormatSpec(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "messagepack-format-spec",
      CFile(s"${module.name}MessagePackFormatSpec.scala"),
      CDir(s"${scalaTestDir(module)}/format/mp"),
      m => Seq(s"${m.basePackage}.model._"))

}

class DefaultModelCartridge(val config: Config) extends ModelCartridge {

  def tasksFor(module: Module): Seq[Task] =
    Seq(packageObject _, jsonFormat _, jsonFormatSpec _, messagePackFormat _, messagePackFormatSpec _)
      .map(_(module))

}
