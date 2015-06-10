package lampetia.test


import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * @author Hossam Karim
 */
object StaticTests extends App {

  import lampetia.sql.dialect.h2._
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit lazy val context: ConnectionSource = {
    val ds = new org.h2.jdbcx.JdbcDataSource
    ds.setUrl("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1")
    connectionSource(ds)
  }

  def run[A](io: IO[A]): Unit = {
    val f = io.run
    f.onSuccess { case v => println(v) }
    f.onFailure { case e => println(e) }
    Await.ready(f, Duration.Inf)
  }

  val q = select("1".literal)

  println(q.sqlString)

  run(q.lifted.read[String])

  context.shutdown()

}
