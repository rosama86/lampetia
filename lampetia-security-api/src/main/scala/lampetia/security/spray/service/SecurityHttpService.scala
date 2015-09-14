package lampetia.security.spray.service

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import lampetia.conf.Configuration
import lampetia.security.spray.route.{SecurityRoute, GroupRoute}
import lampetia.spray.SprayConfiguration
import lampetia.sql.JdbcConnectionSource
import spray.can.Http
import spray.routing._
import lampetia.security.module.SecurityModule
import SecurityModule._

import scala.concurrent.ExecutionContext

/**
 * @author Hossam Karim
 */


class SecurityHttpServiceActor(val conf: SprayConfiguration) extends HttpServiceActor {

  trait DefaultRouteEnv {
    def executionContext: ExecutionContext = SecurityModule.configuration.concurrent.executionContext
    def connectionSource: JdbcConnectionSource = SecurityModule.connectionSource
    def actorRefFactory: ActorSystem = configuration.akka.defaultActorSystem
  }

  val securityRoute = new SecurityRoute with DefaultRouteEnv {}

  val groupRoute = new GroupRoute with DefaultRouteEnv {}

  def route: Route = securityRoute.validateRoute ~ groupRoute.groupRoute

  def apiRoute: Route =
    pathPrefix(conf.apiPrefix) {
      pathPrefix(conf.apiVersion) {
        route
      }
    }

  def receive = runRoute(apiRoute)
}


object SecurityHttpService {

  val sprayConfiguration: SprayConfiguration = new SprayConfiguration {
    override def configuration: Configuration = SecurityModule.configuration
    override def moduleConfigurationPrefix: String = "lampetia.module.security"
  }

  val serviceActor =
      configuration
      .akka
      .defaultActorSystem
      .actorOf(
          Props(classOf[SecurityHttpServiceActor], sprayConfiguration),
          "security-spray-service-actor")

  def serviceStartup(): Unit = {
    implicit val system = configuration.akka.defaultActorSystem
    IO(Http) ! Http.Bind(serviceActor, sprayConfiguration.serviceHost, sprayConfiguration.servicePort)
  }

  def main(args: Array[String]): Unit = {

    serviceStartup()

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        configuration.shutdown()
      }
    })
  }

}
