package lampetia.conf

import akka.actor.ActorSystem
import com.typesafe.config.{ConfigFactory, Config}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

/**
 * @author Hossam Karim
 */

trait Configuration extends Lifecycle {

  val logger = LoggerFactory.getLogger(this.getClass)

  lazy val config: Config = ConfigFactory.load().resolve()

  object akka {
    lazy val defaultActorSystem =
      ActorSystem(s"lampetia-actor-system", config)
  }

  object concurrent {
    lazy val executionContext: ExecutionContext =
      scala.concurrent.ExecutionContext.Implicits.global
  }


  def shutdown(): Unit = {
    logger.info(s"[lampetia] shutdown sequence: begin")
    akka.defaultActorSystem.shutdown()
    logger.info(s"[lampetia] shutdown sequence: done")
  }

}
