package lampetia.cg.cartridge.node

import lampetia.cg.model._
import lampetia.metamodel.{Model, Module}
import lampetia.cg.extensions.Models._
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import lampetia.extensions.Strings._

/**
 * @author Hossam Karim
 */
trait NodeCartridge extends Cartridge[Module] { self =>

  def baseDir(module: Module) = s"${module.name.lispCase}-web"

  def configRoot = "cg.template.node"

  trait NodeFileGenerationTask extends SingleOutputFileGenerationTask {
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

  case class CommonNodeFileGenerationTask(
    module: Module,
    key: String,
    outputFile: CFile,
    outputDir: CDir,
    extraModels: Map[String, Any] = Map.empty[String, Any]) extends NodeFileGenerationTask {
    override def taskModels(models: Seq[Model]): Map[String, Any] =
      super.taskModels(models) ++ extraModels
  }

  def packageJsonTask(module: Module): NodeFileGenerationTask =
    CommonNodeFileGenerationTask(
      module,
      if(module.secure) "package-json-secure" else "package-json",
      CFile("package.json"),
      CDir(s"${baseDir(module)}"))

  def serverTask(module: Module): NodeFileGenerationTask =
    CommonNodeFileGenerationTask(
      module,
      if(module.secure) "server-secure" else "server",
      CFile("server.coffee"),
      CDir(s"${baseDir(module)}"))

  def angularAppTask(module: Module): NodeFileGenerationTask =
    CommonNodeFileGenerationTask(
      module,
      "angular-app",
      CFile("app.coffee"),
      CDir(s"${baseDir(module)}/public/js/app"))

  def indexTask(module: Module): NodeFileGenerationTask =
    CommonNodeFileGenerationTask(
      module,
      "index",
      CFile("index.jade"),
      CDir(s"${baseDir(module)}/views"))

  def loginTask(module: Module): NodeFileGenerationTask =
    CommonNodeFileGenerationTask(
      module,
      "login",
      CFile("login.jade"),
      CDir(s"${baseDir(module)}/views"))

  def viewsEntityTemplateTask(module: Module) = new FileGenerationTask {
    def run(models: Seq[Model]): Seq[FileGeneration] = {
      val tasks = models.entityModels.map { model =>
        val extraModels = Map("model" -> model)
        CommonNodeFileGenerationTask(
          module,
          "views-entity-template",
          CFile(s"${model.modelName.lispCase}s.jade"),
          CDir(s"${baseDir(module)}/views/template"),
          extraModels)
      }
      tasks.flatMap(_.run(models))
    }
  }


}

class DefaultNodeCartridge(val config: Config) extends NodeCartridge {

  def tasksFor(module: Module): Seq[Task] = {
    val packageJson = packageJsonTask(module)
    val server = serverTask(module)
    val angularApp = angularAppTask(module)
    val index = indexTask(module)
    val login = loginTask(module)
    val viewsEntityTemplate = viewsEntityTemplateTask(module)

    val secure = Seq(login)

    val general = Seq(packageJson, server, angularApp, index, viewsEntityTemplate)

    if (module.secure)
      general ++ secure
    else
      general
  }

}
