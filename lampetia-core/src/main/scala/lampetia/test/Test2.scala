package lampetia.test

import lampetia.sql.dialect.h2.jdbc._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * @author Hossam Karim
 */

object Test2 extends App {

  import TestModels._
  implicit val cid: Consume[PersonId] = consume[String].fmap(PersonId)
  implicit val pid: Produce[PersonId] = a => produce(a.value)
  implicit val cd: Consume[PersonData] = (consume[String] ~ consume[String])(PersonData)
  implicit val pd: Produce[PersonData] = a => produce(a.firstName) andThen produce(a.lastName)
  implicit val ce: Consume[Person] = (consume[PersonId] ~ consume[PersonData])(Person)
  implicit val pe: Produce[Person] = a => produce(a.id) andThen produce(a.data)

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

  val m = PersonModel


  val p = Person(PersonId("1"), PersonData("a", "b"))
  val s = 'tmp
  val q = for {
    _ <- "create schema tmp".sql.write
    _ <- "create table tmp.person_t(id text, first_name text, last_name text)".sql.write
    e <- PersonModel.insert(p.data)
    _ <- PersonModel.update(m.data.firstName := "another".bind)(m.id === e.id.bind)
    r <- PersonModel.find
    _ <- PersonModel.delete
    _ <- "drop schema tmp".sql.write
  } yield r

  run(q.transactionally)

  context.shutdown()

}
