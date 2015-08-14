package lampetia.security.spray.route

import lampetia.security.service.GroupService
import spray.routing.HttpService
import scala.util.{Failure, Success}
import spray.httpx.PlayJsonSupport._

/**
 * @author Hossam Karim
 */



trait GroupRoute extends HttpService {

  import lampetia.security.module.SecurityModule

  import SecurityModule.json._

  implicit val ec = SecurityModule.configuration.concurrent.executionContext

  val groupService = new GroupService {}

  def getAll = groupService.findAll(100).run


  def groupRoute =
    pathPrefix("group") {
      get {
        onComplete(getAll) {
          case Success(v) => complete(v)
          case Failure(e) => failWith(e)
        }
      }
    }

}
