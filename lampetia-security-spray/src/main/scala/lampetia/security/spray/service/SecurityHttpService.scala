package lampetia.security.spray.service

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import lampetia.conf.Configuration
import lampetia.security.spray.route.GroupRoute
import lampetia.spray.SprayConfiguration
import spray.can.Http
import spray.routing._
import lampetia.security.module.SecurityModule
import SecurityModule._

/**
 * @author Hossam Karim
 */


class SecurityHttpServiceActor(val apiPrefix: String) extends HttpServiceActor {

  val groupRoute = new GroupRoute {
    def actorRefFactory: ActorSystem = defaultActorSystem
  }

  def mainRoute: Route =
    pathPrefix(apiPrefix) {
      groupRoute.groupRoute
    }

  def receive = runRoute(mainRoute)
}


object SecurityHttpService {

  val sprayConfiguration: SprayConfiguration = new SprayConfiguration {
    override def configuration: Configuration = SecurityModule
    override def moduleConfigurationPrefix: String = "lampetia.module.security"
  }

  val apiPrefix = sprayConfiguration.apiPrefix

  val serviceActor =
      defaultActorSystem
      .actorOf(Props(classOf[SecurityHttpServiceActor], apiPrefix), "security-spray-service-actor")

  def serviceStartup(): Unit = {
    implicit val system = defaultActorSystem
    IO(Http) ! Http.Bind(serviceActor, sprayConfiguration.serviceHost, sprayConfiguration.servicePort)
  }

  def main(args: Array[String]): Unit = {

    serviceStartup()

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        shutdown()
      }
    })
  }

}
