package lampetia.cg.cartridge.scala

import com.typesafe.config.Config
import lampetia.cg.cartridge.sbt.{DefaultSbtCartridge, SbtProject}
import lampetia.cg.extensions.Models._
import lampetia.cg.model._
import lampetia.extensions.Strings
import lampetia.extensions.Strings._
import lampetia.metamodel.Dsl._
import lampetia.metamodel._
import net.ceedubs.ficus.Ficus._

/**
 * @author Hossam Karim
 */

trait ServiceCartridge extends ScalaCartridge {

  def configRoot = "cg.template.service"

  def baseDir(module: Module) = s"${module.name.lispCase}-service"

  def scope = ServiceScope

  def sbtProject(module: Module, dependencies: SbtProject*): SbtProject = {
    val libs = config.as[Seq[String]](s"$configRoot.sbt-libraries") ++ sbtDependencies(module, ServiceScope)
    SbtProject(
      name = s"${module.name}Service",
      dependencies = dependencies,
      libraries = libs,
      plugins = Seq.empty[String],
      extraModels = Map("module" -> module),
      customTemplate = Some("service-build")
    )
  }

  def sbtTasksFor(module: Module): Seq[FileGenerationTask] = {
    val project = sbtProject(module)
    Seq(new DefaultSbtCartridge(config).subProjectBuildTask(project))
  }

  case class ServiceFileGenerationTask(module: Module) extends FileGenerationTask {

    def service(entity: Entity): FileGenerationTask =
      CommonScalaFileGenerationTask(
        module,
        "service",
        CFile(s"${entity.modelName}Service.scala"),
        CDir(s"${scalaDir(module)}/service"),
        _ => Nil,
        Map("model" -> entity))

    def run(models: Seq[Model]): Seq[FileGeneration] =
      models collect { case m: Entity => m} flatMap {
        em => service(em).run(models)
      }
  }

  case class ServiceSpecFileGenerationTask(module: Module) extends FileGenerationTask {

    def serviceSpec(entity: Entity): FileGenerationTask =
      CommonScalaFileGenerationTask(
        module,
        "service-spec",
        CFile(s"${entity.modelName}ServiceSpec.scala"),
        CDir(s"${scalaTestDir(module)}/service"),
        _ => Nil,
        Map("model" -> entity))

    def run(models: Seq[Model]): Seq[FileGeneration] =
      models collect { case m: Entity => m} flatMap {
        em => serviceSpec(em).run(models)
      }
  }

  case class DaoFileGenerationTask(module: Module) extends FileGenerationTask {

    def daoSpecFile(entity: Entity) =
      if (entity.hasReferenceModel)
        CFile(s"${entity.modelName}ChildDaoSpec.scala")
      else
        CFile(s"${entity.modelName}DaoSpec.scala")

    def daoSpec(entity: Entity): FileGenerationTask =
      CommonScalaFileGenerationTask(
        module,
        if (entity.hasReferenceModel) "child-dao-spec" else "dao-spec",
        CFile(s"${entity.modelName}DaoSpec.scala"),
        CDir(s"${scalaTestDir(module)}/dao"),
        m => Seq(s"${m.basePackage}.spec.${m.name}InstanceFactory.{ insertUserId => _, _ }"),
        Map("model" -> entity))

    def run(models: Seq[Model]): Seq[FileGeneration] =
      models collect { case m: Entity => m} flatMap {
        em => daoSpec(em).run(models)
      }
  }

  case class SprayFileGenerationTask(module: Module) extends FileGenerationTask {

    def httpServiceActor: FileGenerationTask =
      CommonScalaFileGenerationTask(
        module,
        "http-service-actor",
        CFile(s"${module.modelName}HttpServiceActor.scala"),
        CDir(s"${scalaDir(module)}/route"))

    def sprayService: FileGenerationTask =
      CommonScalaFileGenerationTask(
        module,
        "spray-service",
        CFile(s"${module.modelName}SprayService.scala"),
        CDir(s"${scalaDir(module)}/route"))

    def route(entity: Entity): FileGenerationTask =
      CommonScalaFileGenerationTask(
        module,
        if (entity.hasReferenceModel) "child-route" else "route",
        CFile(s"${entity.modelName}Route.scala"),
        CDir(s"${scalaDir(module)}/route"),
        m => Seq(
          s"${m.basePackage}.model._",
          s"${m.basePackage}.format.json.${m.name}JsonFormat._"),
        Map("model" -> entity))

    def routeSpec(entity: Entity): FileGenerationTask =
      CommonScalaFileGenerationTask(
        module,
        if (entity.hasReferenceModel) "child-route-spec" else "route-spec",
        CFile(s"${entity.modelName}RouteSpec.scala"),
        CDir(s"${scalaTestDir(module)}/route"),
        m => Seq(
          s"${m.basePackage}.model._",
          s"${m.basePackage}.spec.${m.name}InstanceFactory.{ insertUserId => _, _ }"),
        Map("model" -> entity))

    def run(models: Seq[Model]): Seq[FileGeneration] =
      httpServiceActor.run(models) ++
        sprayService.run(models) ++
        models.collect { case m: Entity => m}
          .flatMap { em =>
          route(em).run(models) ++
            routeSpec(em).run(models)
        }
  }

  def referenceConf(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "application-conf",
      CFile("application.conf"),
      CDir(s"${resourceDir(module)}"))

  def logbackXml(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "logback-xml",
      CFile("logback.xml"),
      CDir(s"${resourceDir(module)}"))

  def referenceTestConf(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "application-conf",
      CFile("application.conf"),
      CDir(s"${resourceTestDir(module)}"))

  def logbackTestXml(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "logback-xml",
      CFile("logback.xml"),
      CDir(s"${resourceTestDir(module)}"))

  def moduleTask(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "module",
      CFile(s"${module.modelName}Module.scala"),
      CDir(s"${scalaDir(module)}/module")
    )

  def configuration(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "configuration",
      CFile(s"${module.modelName}Configuration.scala"),
      CDir(s"${scalaDir(module)}/conf"))

  def testModuleTask(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "test-module",
      CFile(s"${module.modelName}TestModule.scala"),
      CDir(s"${scalaTestDir(module)}/module")
    )

  /*def scalaFormat(module: Module) =
    CommonScalaTask(
      module,
      "scala-format",
      CFile(s"${module.name}ScalaFormat.scala"),
      CDir(s"${scalaDir(module)}/format/scala"))*/

  def sqlFormat(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "sql-format",
      CFile(s"${module.modelName}SqlFormat.scala"),
      CDir(s"${scalaDir(module)}/format"),
      m => Seq(
        s"${m.basePackage}.model._",
        s"${m.basePackage}.format.sql.${m.name}SqlFormat._"))

  def instanceFactory(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "spec-instance-factory",
      CFile(s"${module.modelName}InstanceFactory.scala"),
      CDir(s"${scalaTestDir(module)}/spec"),
      m => Seq(
        s"${m.basePackage}.model._",
        s"${m.basePackage}.spec.${m.name}InstanceFactory.{ insertUserId => _, _ }"))

  def postgresqlModule(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "postgresql-module",
      CFile(s"Postgresql${module.modelName}Module.scala"),
      CDir(s"${scalaDir(module)}/store/postgresql"))

  def ddl(module: Module) =
    CommonScalaFileGenerationTask(
      module,
      "ddl",
      CFile("DDL.scala"),
      CDir(s"${scalaDir(module)}/util"))

  def dao(module: Module) = DaoFileGenerationTask(module)

  def spray(module: Module) = SprayFileGenerationTask(module)

  def service(module: Module) = ServiceFileGenerationTask(module)

  def serviceSpec(module: Module) = ServiceSpecFileGenerationTask(module)
}

class DefaultServiceCartridge(val config: Config) extends ServiceCartridge {

  def tasksFor(module: Module): Seq[Task] =
    Seq(
      configuration _,
      moduleTask _,
      referenceConf _,
      referenceTestConf _,
      logbackTestXml _,
      sqlFormat _,
      ddl _,
      service _,
      testModuleTask _,
      instanceFactory _,
      serviceSpec _ /*,
      logbackXml _,
      //scalaFormat _,
      dao _,
      postgresqlModule _,

      spray _,
      */)
      .map(_(module))
}
