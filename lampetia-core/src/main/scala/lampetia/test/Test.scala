package lampetia.test

import java.util.UUID

import lampetia.model._
import lampetia.sql.ast.{Operator, Operand}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Success
import shapeless._

/**
 * @author Hossam Karim
 */

object Test extends App {

  import lampetia.sql.dsl.dialect.postgres
  import postgres._

  import scala.concurrent.ExecutionContext.Implicits.global

  case class PersonId(value: String)
  case class PersonData(firstName: String, lastName: String)
  case class Person(id: PersonId, data: PersonData)

  implicit object PersonModel
    extends Model[Person]
    with    HasId[Person, PersonId]
    with    HasData[Person, PersonData] {
    type Shape = PersonId :: PersonData :: HNil
    val name = "person"

    def id = property[PersonId]("id", _.id, e => v => e.copy(id = v))
    def parse(stringId: String) = Success(PersonId(stringId))
    def generate = PersonId(UUID.randomUUID.toString)

    object data extends DataModel[Person, PersonData] {

      val firstName: Property[PersonData, String] =
        property[String]("firstName", _.firstName, e => v => e.copy(firstName = v))
          .set(SqlFeature.name("first_name"))
          .set(JsonFeature.name("first-name"))

      val lastName: Property[PersonData, String] =
        property[String]("lastName", _.lastName, e => v => e.copy(lastName = v))
          .set(SqlFeature.name("last_name"))
          .set(SqlFeature.`type`("jsonb"))
          .set(JsonFeature.name("last-name"))

      val properties = Seq(firstName, lastName)
      def get(instance: Person): PersonData = instance.data
      def set(instance: Person, value: PersonData): Person = instance.copy(data = value)

    }

    override def features = Seq(
      SqlFeature.name("person_t"),
      SqlFeature.schema("tmp")
    )

    def combine(hl: PersonId::PersonData::HNil): Person = hl match {
      case id::data::HNil => Person(id, data)
    }



  }

  println(PersonModel.sqlName)

  PersonModel.properties.foreach(p => println(s"${p.name},${p.sqlName}:${p.sqlType},${p.jsonName}"))

  implicit val context: ConnectionSource =
    hikari(
      "org.postgresql.ds.PGSimpleDataSource",
      "localhost", 5432, "jeelona", "admin", "admin", 3, 2000)

  val m = PersonModel

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
    def insert(id: Id)(implicit pid: Produce[Id]): SqlIO[Int] = {
      val m: Operand = model.sqlSchema match {
        case Some(v) => v.identifier dot model
        case None    => model
      }
      insertInto(m).values(id.bind).lifted.writeSqlIO

    }
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
      val init = postgres.update(ms.schemaPrefixed).set(first._1, first._2)
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


  def run[A](io: SqlIO[A]): Unit = {
    val f = io.run
    f.onSuccess { case v => println(v) }
    f.onFailure { case e => println(e) }
    Await.ready(f, Duration.Inf)
  }

  println(PersonModel.echo)


  val p = Person(PersonId("1"), PersonData("a", "b"))
  val s = 'tmp
  val q = for {
    _ <- "create schema tmp".sql.writeSqlIO
    _ <- "create table tmp.person_t(id text, first_name text, last_name text)".sql.writeSqlIO
    e <- PersonModel.insert(p.data)
    _ <- PersonModel.update(liftProperty(m.data.firstName) -> "another".bind)(m.id === e.id.bind)
    r <- PersonModel.find
    _ <- PersonModel.delete
    _ <- "drop schema tmp cascade".sql.writeSqlIO
  } yield r

  run(q.transactionally)

  context.shutdown()

}
