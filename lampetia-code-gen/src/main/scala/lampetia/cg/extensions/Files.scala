package lampetia.cg.extensions

import java.io.{PrintWriter, File}
import java.nio.file.attribute.PosixFilePermission
import java.util

import org.slf4j.LoggerFactory

/**
 * @author Hossam Karim
 */
object Files {

  val log = LoggerFactory.getLogger("FileWriter")

  def printToFile(f: File)(pw: PrintWriter => Unit) {
    val p = new PrintWriter(f)
    try {
      log.info(s"Writing file: ${f.getAbsolutePath}")
      pw(p)
    } finally {
      p.close()
    }
  }

  def makeExecutable(f: File): Unit = {
    val permissions = new util.HashSet[PosixFilePermission]()
    permissions.add(PosixFilePermission.OWNER_READ)
    permissions.add(PosixFilePermission.OWNER_WRITE)
    permissions.add(PosixFilePermission.OWNER_EXECUTE)
    permissions.add(PosixFilePermission.GROUP_READ)
    permissions.add(PosixFilePermission.GROUP_WRITE)
    permissions.add(PosixFilePermission.GROUP_EXECUTE)
    permissions.add(PosixFilePermission.OTHERS_READ)
    permissions.add(PosixFilePermission.OTHERS_EXECUTE)
    java.nio.file.Files.setPosixFilePermissions(java.nio.file.Paths.get(f.toURI), permissions)
  }

}
