package lampetia.security.spray.route

import akka.actor.ActorSystem
import lampetia.security.module.SecurityTestModule
import lampetia.sql.JdbcConnectionSource

import scala.concurrent.ExecutionContext

/**
 * @author Hossam Karim
 */

trait TestRouteEnv {

  def executionContext: ExecutionContext = SecurityTestModule.configuration.concurrent.executionContext
  def connectionSource: JdbcConnectionSource = SecurityTestModule.connectionSource
  def actorRefFactory: ActorSystem = SecurityTestModule.configuration.akka.defaultActorSystem

}
