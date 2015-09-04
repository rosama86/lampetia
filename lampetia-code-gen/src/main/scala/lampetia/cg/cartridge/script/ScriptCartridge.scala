package lampetia.cg.cartridge.script

import lampetia.cg.model._
import lampetia.extensions.Strings
import lampetia.metamodel.{Model, Module}
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import Strings._


/**
 * @author Hossam Karim
 */



trait ScriptCartridge extends Cartridge[Module] { self =>

  def baseDir(module: Module): String = s"${module.name.lispCase}-etc"

  def resourceDir(module: Module) = s"${baseDir(module)}/script"

  def configRoot = "cg.template.script"

  trait ScriptFileGenerationTask extends SingleOutputFileGenerationTask {
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

  case class CommonScriptFileGenerationTask(module: Module, key: String, outputFile: CFile, outputDir: CDir) extends ScriptFileGenerationTask

  def triggersTask(module: Module): ScriptFileGenerationTask =
    CommonScriptFileGenerationTask(
      module,
      "triggers",
      CFile("triggers.sql"),
      CDir(s"${resourceDir(module)}/postgresql"))

}

class DefaultScriptCartridge(val config: Config) extends ScriptCartridge {

  def tasksFor(module: Module): Seq[Task] = {
    val triggers = triggersTask(module)
    Seq(triggers)
  }

}
