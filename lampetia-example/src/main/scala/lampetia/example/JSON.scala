package lampetia.example

import lampetia.example.model._
import lampetia.example.module.ExampleModule
import lampetia.meta._
import org.joda.time.DateTime
import play.api.libs.json.{Format, Json}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}


object JSON extends App {

  import lampetia.meta.feature._
  import ExampleModule.dialect._

  implicit val connectionSource = ExampleModule.connectionSource
  implicit val executionContext = ExampleModule.executionContext

  // define a document model
  case class Data(i: Int, s: String, d: DateTime)
  case class Document(id: String, content: Data)

  val jsonb = "jsonb"

  // define a document metamodel
  object DocumentModel
    extends Model[Document]
    with HasId[Document, String] {
    def modelName: String = "Document"
    def id: Property[String] = property[String]("id")
    def content: Property[Data] = property[Data]("content").set(sql.`type`(jsonb))
    override def properties = Seq(id, content)
  }

  // provide a Document reader
  implicit val dataFormat: Format[Data]  = Json.format[Data]
  implicit val dataConsume: Consume[Data] = consume[String].fmap(s => Json.parse(s).as[Data])
  implicit val documentConsume: Consume[Document] = (consume[String] and consume[Data])(Document)

  // provide a Document writer
  implicit val dataProduce: Produce[Data] = data => produce(Json.stringify(Json.toJson(data)))
  implicit val documentProduce: Produce[Document] = doc =>  produce(doc.id) andThen produce(doc.content)

  val m = DocumentModel

  def simpleActions(): Unit = {

    val actions = for {
      _ <- m.create.transactionally
      _ <- m.insert(m.id := "d1".bind, m.content := Data(1, "a", DateTime.now()).bind.cast(typeName(jsonb)))
      d <- m.findOne(m.id === "d1".bind)
      _ <- m.drop(cascade = false)
    } yield d

    val result = actions.run

    result.onComplete {
      case Success(Some(doc)) => println(doc)
      case Success(None)      => println("not found")
      case Failure(e)         => println(e.getMessage)
    }

    Await.ready(result, Duration.Inf)
  }

  def jsonQuery(): Unit = {
    val action = Q.select(m.content -> "i".literal).from(m).lifted.read[Int]
    val result = action.run
    result.onComplete {
      case Success(v) => println(v)
      case Failure(e) => println(e.getMessage)
    }
    Await.ready(result, Duration.Inf)
  }


  simpleActions()

  ExampleModule.terminate()


}
