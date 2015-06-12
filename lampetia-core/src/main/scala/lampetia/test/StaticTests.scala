package lampetia.test


import lampetia.model.JSON
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * @author Hossam Karim
 */
object StaticTests extends App {

  import lampetia.sql.dialect._

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit lazy val h2context: h2.jdbc.ConnectionSource = {
    val ds = new org.h2.jdbcx.JdbcDataSource
    ds.setUrl("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1")
    h2.jdbc.connectionSource(ds)
  }

  implicit lazy val pgcontext: postgres.jdbc.ConnectionSource =
    postgres.jdbc.hikari(
      "org.postgresql.ds.PGSimpleDataSource",
      "localhost", 5432, "jeelona", "admin", "admin", 3, 2000)

  def h2run[A](io: h2.jdbc.IO[A]): Unit = {
    val f = io.run
    f.onSuccess { case v => println(v) }
    f.onFailure { case e => println(e) }
    Await.ready(f, Duration.Inf)
  }

  def pgrun[A](io: postgres.jdbc.IO[A]): Unit = {
    val f = io.run
    f.onSuccess { case v => println(v) }
    f.onFailure { case e => println(e) }
    Await.ready(f, Duration.Inf)
  }

  def h2t1(): Unit = {
    import h2.jdbc._
    val q = select("1".literal)
    println(q.sqlString)
    h2run(q.lifted.read[String])
  }
  def pgt1(): Unit = {
    import postgres.jdbc._
    val q = select("1".bind).limit(1.bind).offset(3.bind)
    println(q.sqlString)
    pgrun(q.lifted.read[String])
  }

  case class PlayJson(value: JsValue) extends AnyVal with JSON {
    def stringify: String = Json.stringify(value)
  }

  def pgt2(): Unit = {
    import postgres.jdbc._
    val jsonb = "jsonb".typeName
    implicit val consumeJson: Consume[JSON] = consume[String].fmap(js => Json.parse(js)).fmap(PlayJson)
    implicit val produceJson: Produce[JSON] = a => produce(a.stringify)

    val v = PlayJson(Json.obj("key" -> 1))

    val q = select(v.bind.cast(jsonb))
    println(q.sqlString)
    pgrun(q.lifted.read[JSON])
  }

  //h2t1()
  //pgt1()
  pgt2()




  h2context.shutdown()
  pgcontext.shutdown()

}
