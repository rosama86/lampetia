package lampetia.security.conf

import lampetia.conf.{Configuration, Lifecycle}
import scala.concurrent.ExecutionContext

/**
 * @author Hossam Karim
 */

trait SecurityConfiguration extends Lifecycle { self: Configuration =>

  lazy val schema: String =
    config.getString("lampetia.module.security.schema")

  abstract override def shutdown(): Unit = {
    logger.info(s"[security] shutdown sequence: begin")
    logger.info(s"[security] shutdown sequence: done")
    super.shutdown()
  }

}

