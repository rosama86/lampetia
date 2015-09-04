package lampetia.cg

import java.io.File
import java.net.URI

import com.typesafe.config.Config
import lampetia.cg.extensions.Files._
import lampetia.cg.model.{CopyTask, FileGeneration, FileGenerationTask, Task}
import lampetia.metamodel.{BaseGenerationDirectory, Model, Module}
import net.ceedubs.ficus.Ficus._
import org.apache.commons.io.FileUtils
import org.fusesource.scalate.TemplateEngine
import org.fusesource.scalate.support.URLTemplateSource
import org.slf4j.LoggerFactory
/**
 * @author Hossam Karim
 */

class Generator(config: Config, moduleDir: String, tasks: Seq[Task], module: Module, models: Seq[Model]) {

  val log = LoggerFactory.getLogger("CodeGenerator")

  val engine = new TemplateEngine()
  engine.classpath = config.as[String]("cg.engine.classpath")
  engine.workingDirectory = new File(config.as[String]("cg.engine.working-dir"))
  engine.escapeMarkup = false

  val baseDirectory = module.options.collectFirst {
    case BaseGenerationDirectory(path) => path
  }.getOrElse(s"""${config.as[String]("cg.generator.base-directory")}/$moduleDir""")



  def cacheTemplate(path: String): Unit = {
    log.info(s"Caching Template: $path")
    val url = Thread.currentThread().getContextClassLoader.getResource(path)
    val template = new URLTemplateSource(url)
    engine.compile(template)
  }

  def format(): Unit = {
    val extraOptions = config.as[List[String]]("cg.generator.scalariform-options")
    val args = List("--scalaVersion=2.11", "--recurse") ++ extraOptions ++ List(baseDirectory)
    scalariform.commandline.Main.process(args.toArray)
  }

  def generate(formatScalaSource: Boolean = true): Unit = {
    // cache all the templates
    //tasks.flatMap(_.templates).foreach(cacheTemplate)

    // ensure baseDirectory is created
    log.info(s"Creating Directory: $baseDirectory")
    new File(baseDirectory).mkdirs()


    def write(generation: FileGeneration): File = {
      val dir = s"$baseDirectory/${generation.directory.value}"
      new File(dir).mkdirs()
      val templateModels = generation.models + ("config" -> config)
      val code = engine.layout(generation.template.value, templateModels)
      val file = new File(s"$dir/${generation.filename.value}")
      printToFile(file)(pw => pw.println(code))
      file
    }

    // invoke the tasks
    //tasks.foreach(_.run(models).foreach(write))
    tasks.collect {
      case task: FileGenerationTask =>
        val files: Seq[File] = task.run(models).map(write)
        task.done(files)

      case task: CopyTask =>
        val resourceDir: URI = Thread.currentThread().getContextClassLoader.getResource(".").toURI
        val moduleDir: URI = new File(baseDirectory).toURI
        FileUtils.copyDirectory(task.source(resourceDir), task.destination(moduleDir))
    }

    // format the source code
    if(formatScalaSource) format()
  }




}
