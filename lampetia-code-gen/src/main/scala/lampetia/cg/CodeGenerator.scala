package lampetia.cg

import com.typesafe.config.ConfigFactory
import lampetia.cg.cartridge.deploy.DefaultDeployCartridge
import lampetia.cg.cartridge.node.DefaultNodeCartridge
import lampetia.cg.cartridge.sbt.{DefaultSbtCartridge, SbtProject}
import lampetia.cg.cartridge.scala.{DefaultModelCartridge, DefaultServiceCartridge}
import lampetia.cg.cartridge.script.DefaultScriptCartridge
import lampetia.cg.cartridge.util.DefaultUtilCartridge
import lampetia.cg.model.Task
import lampetia.extensions.Strings._
import lampetia.metamodel.{Model, Module}

/**
 * @author Hossam Karim
 */

trait CodeGenerator {

  val config = ConfigFactory.defaultReference()

  def modelCartridge = new DefaultModelCartridge(config)
  def serviceCartridge = new DefaultServiceCartridge(config)
  def sbtCartridge = new DefaultSbtCartridge(config)
  def scriptCartridge = new DefaultScriptCartridge(config)
  def nodeCartridge = new DefaultNodeCartridge(config)
  def deployCartridge = new DefaultDeployCartridge(config)
  def utilCartridge = new DefaultUtilCartridge(config)

  def generator(moduleDir: String, tasks: Seq[Task], module: Module, models: Seq[Model]) =
    new Generator(config, moduleDir, tasks, module, models)

  def modelGenerator(module: Module, models: Seq[Model]): Generator = {
    val modelProject = modelCartridge.sbtProject(module)
    val rootProject = SbtProject(name = module.name, subProjects = Seq(modelProject))
    val tasks = modelCartridge.tasksFor(module) ++ sbtCartridge.tasksFor(rootProject)
    generator(module.name.lispCase, tasks, module, models)
  }

  def serviceGenerator(module: Module, models: Seq[Model]): Generator = {
    val modelProject = modelCartridge.sbtProject(module)
    //println(modelProject)
    val serviceProject = serviceCartridge.sbtProject(module, modelProject)
    //println(serviceProject)
    val rootProject = SbtProject(name = module.name, subProjects = Seq(modelProject, serviceProject))
    val tasks =
      modelCartridge.tasksFor(module) ++
        serviceCartridge.tasksFor(module) ++
        sbtCartridge.tasksFor(rootProject)
    generator(module.name.lispCase, tasks, module, models)
  }

  def nodeGenerator(module: Module, models: Seq[Model]): Generator = {
    val tasks = nodeCartridge.tasksFor(module)
    generator(module.name.lispCase, tasks, module, models)
  }

  def deployGenerator(module: Module, models: Seq[Model]): Generator = {
    val tasks = deployCartridge.tasksFor(module)
    generator(module.name.lispCase, tasks, module, models)
  }

  def utilGenerator(module: Module, models: Seq[Model]): Generator = {
    val tasks = utilCartridge.tasksFor(module)
    generator(module.name.lispCase, tasks, module, models)
  }

  def scriptGenerator(module: Module, models: Seq[Model]): Generator = {
    val tasks = scriptCartridge.tasksFor(module)
    generator(module.name.lispCase, tasks, module, models)
  }

}

object CodeGenerator extends CodeGenerator

