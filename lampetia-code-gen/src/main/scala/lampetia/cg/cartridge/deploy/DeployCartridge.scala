package lampetia.cg.cartridge.deploy

/**
 * @author Hossam Karim
 */

import java.io.File
import java.net.URI

import lampetia.cg.model._
import lampetia.extensions.Strings._
import lampetia.metamodel.{Model, Module}
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

/**
 * @author Hossam Karim
 */

trait DeployCartridge extends Cartridge[Module] { self =>

  def baseDir(module: Module) = s"${module.name.lispCase}-deploy"

  def configRoot = "cg.template.deploy"

  trait DeployFileGenerationTask extends SingleOutputFileGenerationTask {
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

  case class CommonDeployFileGenerationTask(module: Module, key: String, outputFile: CFile, outputDir: CDir) extends DeployFileGenerationTask

  def readmeTask(module: Module): DeployFileGenerationTask =
    CommonDeployFileGenerationTask(
      module,
      "readme",
      CFile("README.md"),
      CDir(s"${baseDir(module)}"))

  def vagrantFileTask(module: Module): DeployFileGenerationTask =
    CommonDeployFileGenerationTask(
      module,
      "vagrant-file",
      CFile("Vagrantfile"),
      CDir(s"${baseDir(module)}/vagrant"))

  def ansibleStaticFilesTask(module: Module): CopyTask =
    new CopyTask {
      def source(resourceDir: URI): File = {
        val norm = s"${resourceDir.getRawPath}/deploy/ansible/static/"
        new File(norm)
      }

      def destination(moduleDir: URI): File = {
        val norm = s"${moduleDir.getRawPath}/${baseDir(module)}/ansible"
        new File(norm)
      }
    }

  def ansibleProdTask(module: Module): DeployFileGenerationTask =
    CommonDeployFileGenerationTask(
      module,
      "ansible-prod",
      CFile("prod"),
      CDir(s"${baseDir(module)}/ansible"))

  def ansibleDevTask(module: Module): DeployFileGenerationTask =
    CommonDeployFileGenerationTask(
      module,
      "ansible-dev",
      CFile("dev"),
      CDir(s"${baseDir(module)}/ansible"))

  def ansibleTopologyVarsTask(module: Module): DeployFileGenerationTask =
    CommonDeployFileGenerationTask(
      module,
      "ansible-topology-vars",
      CFile("main.yml"),
      CDir(s"${baseDir(module)}/ansible/roles/topology/vars"))

}

class DefaultDeployCartridge(val config: Config) extends DeployCartridge {

  def tasksFor(module: Module): Seq[Task] = {
    val readme = readmeTask(module)
    val vagrantFile = vagrantFileTask(module)
    val ansibleStaticFiles = ansibleStaticFilesTask(module)
    val ansibleProd = ansibleProdTask(module)
    val ansibleDev = ansibleDevTask(module)
    val ansibleTopologyVars = ansibleTopologyVarsTask(module)
    Seq(readme, vagrantFile, ansibleProd, ansibleDev, ansibleTopologyVars, ansibleStaticFiles)
  }

}
