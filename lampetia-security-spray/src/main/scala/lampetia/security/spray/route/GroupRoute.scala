package lampetia.security.spray.route

import lampetia.security.service.GroupService
import spray.routing.HttpService

import scala.util.{Failure, Success}

/**
 * @author Hossam Karim
 */



trait GroupRoute extends HttpService {

  import lampetia.security.module.SecurityModule._

  implicit val ec = executionContext

  val groupService = new GroupService {}

  def getAll = groupService.findAll(100).run


  def groupRoute =
    pathPrefix("group") {
      get {
        onComplete(getAll) {
          case Success(v) => complete(v.map(g => g.id).mkString("[", ",", "]"))
          case Failure(e) => failWith(e)
        }
      }
    }

}
