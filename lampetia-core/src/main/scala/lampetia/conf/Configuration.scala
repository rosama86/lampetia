package lampetia.conf

import com.typesafe.config.{ConfigFactory, Config}
import org.slf4j.LoggerFactory

/**
 * @author Hossam Karim
 */

trait Configuration extends Lifecycle {

  protected val logger = LoggerFactory.getLogger(this.getClass)

  def config: Config = ConfigFactory.load().resolve()

  def shutdown(): Unit = {
    logger.info(s"[lampetia] shutdown sequence: begin")
    //defaultActorSystem.shutdown()
    logger.info(s"[lampetia] shutdown sequence: done")
  }

}
