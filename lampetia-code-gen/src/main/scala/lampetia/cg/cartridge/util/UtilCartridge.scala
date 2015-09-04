package lampetia.cg.cartridge.util

import java.io.File

import lampetia.cg.extensions.Files
import lampetia.cg.model._
import lampetia.metamodel.{Model, Module}
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

/**
 * @author Hossam Karim
 */

trait UtilCartridge extends Cartridge[Module] { self =>

  def configRoot = "cg.template.util"

  trait UtilFileGenerationTask extends SingleOutputFileGenerationTask {
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

  case class CommonUtilFileGenerationTask(
    module: Module,
    key: String,
    outputFile: CFile,
    outputDir: CDir,
    doneFn: Option[Seq[File] => Unit] = None) extends UtilFileGenerationTask {
    override def done(files: Seq[File]): Unit = doneFn match {
      case Some(fn) => fn(files)
      case None     => ()
    }
  }

  def readmeTask(module: Module): UtilFileGenerationTask =
    CommonUtilFileGenerationTask(
      module,
      "readme",
      CFile("README.md"),
      CDir("."))

  def gitignoreTask(module: Module): UtilFileGenerationTask =
    CommonUtilFileGenerationTask(
      module,
      "gitignore",
      CFile(".gitignore"),
      CDir("."))

  def haproxyTask(module: Module): UtilFileGenerationTask =
    CommonUtilFileGenerationTask(
      module,
      "haproxy",
      CFile("haproxy-local.cfg"),
      CDir("."))

  def copyToDeployTask(module: Module): UtilFileGenerationTask = {
    def done(files: Seq[File]): Unit = {
      files.foreach(Files.makeExecutable)
    }
    val task = CommonUtilFileGenerationTask(
      module,
      "copy-to-deploy",
      CFile("copy-to-deploy.sh"),
      CDir("."),
      Some(done))

    task
  }

}

class DefaultUtilCartridge(val config: Config) extends UtilCartridge {

  def tasksFor(module: Module): Seq[Task] = {
    val readme = readmeTask(module)
    val gitignore = gitignoreTask(module)
    val haproxy = haproxyTask(module)
    val copyToDeploy = copyToDeployTask(module)
    Seq(readme, gitignore, haproxy, copyToDeploy)
  }

}
