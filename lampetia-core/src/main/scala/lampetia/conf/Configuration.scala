package lampetia.conf

import akka.actor.ActorSystem
import com.typesafe.config.{ConfigFactory, Config}
import org.slf4j.LoggerFactory

/**
 * @author Hossam Karim
 */

trait Configuration extends Lifecycle {

  protected val logger = LoggerFactory.getLogger(this.getClass)

  lazy val config: Config = ConfigFactory.load().resolve()

  lazy val defaultActorSystem =
    ActorSystem(s"lampetia-actor-system", config)

  def shutdown(): Unit = {
    logger.info(s"[lampetia] shutdown sequence: begin")
    defaultActorSystem.shutdown()
    logger.info(s"[lampetia] shutdown sequence: done")
  }

}
