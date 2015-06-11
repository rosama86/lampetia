package lampetia.test


import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * @author Hossam Karim
 */
object StaticTests extends App {

  import lampetia.sql.dialect._
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit lazy val h2context: h2.ConnectionSource = {
    val ds = new org.h2.jdbcx.JdbcDataSource
    ds.setUrl("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1")
    h2.connectionSource(ds)
  }

  implicit lazy val pgcontext: postgres.ConnectionSource =
    postgres.hikari(
      "org.postgresql.ds.PGSimpleDataSource",
      "localhost", 5432, "jeelona", "admin", "admin", 3, 2000)

  def h2run[A](io: h2.IO[A]): Unit = {
    val f = io.run
    f.onSuccess { case v => println(v) }
    f.onFailure { case e => println(e) }
    Await.ready(f, Duration.Inf)
  }

  def pgrun[A](io: postgres.IO[A]): Unit = {
    val f = io.run
    f.onSuccess { case v => println(v) }
    f.onFailure { case e => println(e) }
    Await.ready(f, Duration.Inf)
  }

  def h2t1(): Unit = {
    import h2._
    val q = select("1".literal)
    println(q.sqlString)
    h2run(q.lifted.read[String])
  }
  def pgt1(): Unit = {
    import postgres._
    val q = select("1".bind).limit(1.bind).offset(3.bind)
    println(q.sqlString)
    pgrun(q.lifted.read[String])
  }

  h2t1()
  pgt1()




  h2context.shutdown()
  pgcontext.shutdown()

}
