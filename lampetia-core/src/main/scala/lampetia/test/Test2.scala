package lampetia.test

import lampetia.model.{HasData, HasRef, HasId, Model}
import lampetia.sql.ast.{Operator, Operand}
import shapeless._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * @author Hossam Karim
 */

object Test2 extends App {

  import lampetia.sql.dsl.dialect.h2._
  import scala.concurrent.ExecutionContext.Implicits.global
  import TestModels._
  implicit val cid: Consume[PersonId] = consume[String].fmap(PersonId)
  implicit val pid: Produce[PersonId] = a => produce(a.value)
  implicit val cd: Consume[PersonData] = (consume[String] ~ consume[String])(PersonData)
  implicit val pd: Produce[PersonData] = a => produce(a.firstName) andThen produce(a.lastName)
  implicit val ce: Consume[Person] = (consume[PersonId] ~ consume[PersonData])(Person)
  implicit val pe: Produce[Person] = a => produce(a.id) andThen produce(a.data)

  implicit class ModelOps[E](val model: Model[E]) extends AnyVal with ModelSchema[E] with Find[E] {
    def echo: String = "model"
  }
  implicit class Model2Ops[E, Id](
                                   val model: Model[E] with
                                     HasId[E, Id] {type Shape = Id::HNil})
    extends AnyVal with ModelSchema[E] with Find[E] {
    def echo: String = "model with id"
    def instance(id: Id): E = model.combine(id::HNil)
    def insert(id: Id)(implicit pid: Produce[Id]): SqlIO[Int] =
      insertInto(schemaPrefixed).values(id.bind).lifted.writeSqlIO
  }
  trait ModelSchema[E] extends Any {
    def model: Model[E]
    def schemaPrefixed: Operand = model.sqlSchema match {
      case Some(v) => v.identifier dot model
      case None    => model
    }
  }

  trait Find[E] extends Any { ms: ModelSchema[E] =>

    def find(implicit ce: Consume[E]): SqlIO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).lifted.readSqlIO[E]

    def find[F <: Operator](filter: F)(implicit ce: Consume[E]): SqlIO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).where(filter).lifted.readSqlIO[E]

    def findOne[F <: Operator](filter: F)(implicit ce: Consume[E]): SqlIO[Option[E]] =
      find(filter).map(_.headOption)

  }

  trait Delete[E] extends Any { ms: ModelSchema[E] =>

    def delete: SqlIO[Int] =
      deleteFrom(ms.schemaPrefixed).lifted.writeSqlIO

    def delete[F <: Operator](filter: F): SqlIO[Int] =
      deleteFrom(ms.schemaPrefixed).where(filter).lifted.writeSqlIO
  }

  trait Update[E] extends Any { ms: ModelSchema[E] =>

    def update[F <: Operator](first: (Operand, Operand), next: (Operand, Operand)*)(filter: F): SqlIO[Int] = {
      val init = Q.update(ms.schemaPrefixed).set(first._1, first._2)
      next.foldLeft(init)( (upd, c) => upd.set(c._1, c._2)).where(filter).liftedDebug.writeSqlIO
    }

  }

  implicit class Model3Ops[E, Id, R](
                                      val model: Model[E] with
                                        HasId[E, Id] with
                                        HasRef[E, R] {type Shape = Id::R::HNil})
    extends AnyVal with ModelSchema[E] with Find[E] with Delete[E] with Update[E] {
    def echo: String = "model with id"
    def instance(id: Id, ref: R): E = model.combine(id::ref::HNil)
    def find(id: Id)(implicit pid: Produce[Id], ce: Consume[E]): SqlIO[Option[E]] =
      find(model.id === id.bind).map(_.headOption)

    def insert(id: Id, ref: R)(implicit pid: Produce[Id], pref: Produce[R]): SqlIO[Int] = {
      val ps = model.id +: model.ref.properties
      val vs = ps.map(_ => ?)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(ref).writeSqlIO
    }
  }
  implicit class Model4Ops[E, Id, D](
                                      val model: Model[E] with
                                        HasId[E, Id] with
                                        HasData[E, D] {type Shape = Id::D::HNil})
    extends AnyVal with ModelSchema[E] with Find[E] with Delete[E] with Update[E] {
    def echo: String = "model with id with data"
    def instance(id: Id, data: D): E = model.combine(id::data::HNil)
    def insert(id: Id, data: D)(implicit pid: Produce[Id], pdata: Produce[D]): SqlIO[Int] = {
      val ps = model.id +: model.data.properties
      val vs = ps.map(_ => ?)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(data).writeSqlIO
    }
    def insert(data: D)(implicit pid: Produce[Id], pdata: Produce[D]): SqlIO[E] = {
      val ps = model.id +: model.data.properties
      val vs = ps.map(_ => ?)
      val id = model.generate
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(data).writeSqlIO.flatMap {
        case i if i > 0 => IOPure(model.combine(id::data::HNil))
        case _          => IOFailed(new Exception("No Instance"))
      }
    }
  }
  implicit class Model5Ops[E, Id, D, R](
                                         val model: Model[E] with
                                           HasId[E, Id] with
                                           HasRef[E, R] with
                                           HasData[E, D] {type Shape = Id::R::D::HNil})
    extends AnyVal with ModelSchema[E] with Find[E] with Delete[E] with Update[E] {
    def echo: String = "model with id with data with ref"
    def instance(id: Id, ref: R, data: D): E = model.combine(id::ref::data::HNil)
    def insert(id: Id, ref: R, data: D)(implicit pid: Produce[Id], pref: Produce[R], pdata: Produce[D]): SqlIO[Int] = {
      val ps = model.id +: (model.ref.properties ++ model.data.properties)
      val vs = ps.map(_ => ?)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(ref).set(data).writeSqlIO
    }
  }

  implicit lazy val context: ConnectionSource = {
    val ds = new org.h2.jdbcx.JdbcDataSource
    ds.setUrl("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1")
    connectionSource(ds)
  }

  def run[A](io: SqlIO[A]): Unit = {
    val f = io.run
    f.onSuccess { case v => println(v) }
    f.onFailure { case e => println(e) }
    Await.ready(f, Duration.Inf)
  }

  val p = Person(PersonId("1"), PersonData("a", "b"))
  val s = 'tmp
  val q = for {
    _ <- "create schema tmp".sql.writeSqlIO
    _ <- "create table tmp.person_t(id text, first_name text, last_name text)".sql.writeSqlIO
    _ <- PersonModel.insert(PersonData("a", "b"))
    r <- PersonModel.find
    _ <- "drop schema tmp".sql.writeSqlIO
  } yield r

  run(q.transactionally)

  context.shutdown()

}
