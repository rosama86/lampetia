package lampetia.spray

import spray.can.Http
import akka.actor.{ActorSystem, ActorRef}
import akka.io.IO
import lampetia.conf.Configuration

/**
 * @author Hossam Karim
 */
trait SprayService {

  def configuration: Configuration
  def sprayConfiguration: SprayConfiguration

  def serviceActor: ActorRef

  protected implicit def system: ActorSystem


  def serviceStartup(): Unit = {
    IO(Http) ! Http.Bind(serviceActor, sprayConfiguration.serviceHost, sprayConfiguration.servicePort)
  }

  def serviceShutdown(): Unit = {
    configuration.shutdown()
  }

}
