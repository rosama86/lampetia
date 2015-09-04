package lampetia.cg

import java.io.File
import java.net.URI
import java.nio.file.Path

import lampetia.metamodel.Model
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

/**
 * @author Hossam Karim
 */

package object model {

  trait Cartridge[A] {
    def configRoot: String
    def config: Config
    def tasksFor(cartridgeModel: A): Seq[Task]
    def tasksFor(modules: A*): Seq[Task] = modules.flatMap(tasksFor)
    def excluded: Seq[String] =
      config.getAs[List[String]](s"$configRoot.exclude").getOrElse(Seq())
    def whenAvailable(localKey: String)(f: => FileGeneration): Option[FileGeneration] =
      if (excluded.contains(localKey))
        None
      else
        Some(f)
  }

  case class CTemplate(value: String) extends AnyVal
  case class CFile(value: String) extends AnyVal
  case class CDir(value: String) extends AnyVal

  case class FileGeneration(template: CTemplate, filename: CFile, directory: CDir, models: Map[String, Any])

  trait Task

  trait CopyTask extends Task {
    def source(resourceDir: URI): File
    def destination(moduleDir: URI): File
  }


  trait FileGenerationTask extends Task {

    def run(models: Seq[Model]): Seq[FileGeneration]
    def done(files: Seq[File]): Unit = ()

  }

  trait SingleOutputFileGenerationTask extends FileGenerationTask {
    def runSingle(models: Seq[Model]): Option[FileGeneration]
    def run(models: Seq[Model]): Seq[FileGeneration] =
      runSingle(models).map(List(_)).getOrElse(Nil)
  }



}
