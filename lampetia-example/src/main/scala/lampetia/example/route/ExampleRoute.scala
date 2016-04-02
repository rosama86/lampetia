package lampetia.example.route

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import lampetia.example.model.{CompanyData, CompanyId}
import lampetia.example.module.ExampleModule
import lampetia.example.service.CompanyService
import lampetia.sql.JdbcConnectionSource
import spray.can.Http
import spray.http.StatusCodes
import spray.routing.{HttpService, HttpServiceActor}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
  * @author Hossam Karim
  */

trait DefaultHttpService {

  implicit def executionContext: ExecutionContext = ExampleModule.configuration.concurrent.executionContext

  implicit def connectionSource: JdbcConnectionSource = ExampleModule.connectionSource

  def actorRefFactory: ActorSystem = ExampleModule.configuration.akka.defaultActorSystem
}

trait ExampleRoute extends HttpService with DefaultHttpService {

  import ExampleModule.json._
  import spray.httpx.PlayJsonSupport._

  def companyService: CompanyService

  def company = {
    pathPrefix("company") {
      post {
        entity(as[CompanyData]) { data =>
          onComplete(companyService.insert(data).run) {
            case Success(model) => complete(StatusCodes.Created, model)
            case Failure(e)     => failWith(e)
          }
        }
      } ~
      get {
        pathEndOrSingleSlash {
          onComplete(companyService.find.run)  {
            case Success(model) => complete(StatusCodes.OK, model)
            case Failure(e)     => failWith(e)
          }
        } ~
        path(Segment) { id =>
          val companyId = CompanyId(id)
          onComplete(companyService.findOne(companyId).run) {
            case Success(Some(model)) => complete(StatusCodes.OK, model)
            case Success(None)        => complete(StatusCodes.NotFound)
            case Failure(e)           => failWith(e)
          }
        }
      }
    }
  }


  def route = {
    company
  }

}


class ExampleHttpServiceActor extends HttpServiceActor {


  val exampleRoute = new ExampleRoute {
    val companyService = new CompanyService {}
  }

  def api =
    pathPrefix("api") {
      pathPrefix("1.0") {
        exampleRoute.route
      }
    }

  def receive = runRoute(api)

}

object ExampleHttpService {


  val serviceActor =
    ExampleModule
      .configuration
      .akka
      .defaultActorSystem
      .actorOf(Props(classOf[ExampleHttpServiceActor]), "example-spray-service-actor")

  def serviceStartup(): Unit = {
    implicit val system = ExampleModule.configuration.akka.defaultActorSystem
    IO(Http) ! Http.Bind(serviceActor, "0.0.0.0", 9000)
  }

  def main(args: Array[String]): Unit = {

    import ExampleModule.dialect._
    import ExampleModule.sql._
    implicit val ec = ExampleModule.configuration.concurrent.executionContext
    implicit val cs = ExampleModule.connectionSource

    CompanyModel.create.flatMap(_ => EmployeeModel.create).run


    serviceStartup()

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        val down = for {
          _ <- "drop table employee cascade".sql.write
          _ <- "drop table company cascade".sql.write
        } yield ()
        down.run.onComplete {
          case _ =>
            ExampleModule.configuration.shutdown()
        }
      }
    })
  }

}