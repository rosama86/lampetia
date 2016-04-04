## lampetia

Lampetia is a simple SQL generator and runner

### Minimal Example

The following example demonstrates using plain SQL statements against a Postgresql database

```scala
package lampetia.example

import lampetia.sql.ConnectionSourceFactories

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}


// sample model
case class Person(id: String, name: String)

object Minimal {

  // import the SQL dialect
  import lampetia.sql.dialect.postgresql.Postgresql._

  // expose an implicit execution context
  import scala.concurrent.ExecutionContext.Implicits.global

  // create a connection source
  implicit lazy val connectionSource =
    ConnectionSourceFactories.hikariFromDataSourceClass(
      dataSourceClassName = "org.postgresql.ds.PGSimpleDataSource",
      serverName = "localhost",
      portNumber = 5432,
      databaseName = "db",
      user = "admin",
      password = "admin")

  // tell lampetia how to map a row into a Person model
  implicit lazy val consumePerson: Consume[Person] = (consume[String] and consume[String])(Person)

  // create table action
  val createTable = sql"create table person(id text, name text)".write

  // drop table action
  val dropTable = sql"drop table person".write

  // insert person action, notice the return type
  def insertPerson(p: Person): IO[Int] = sql"insert into person values (${p.id}, ${p.name})".write

  def main(args: Array[String]): Unit = {

    // a query parameter
    val person = Person("p1", "someone")

    // select person query, notice the return type
    val select: IO[Option[Person]] =
      sql"select id, name from person where id=${person.id}".read[Person].map(_.headOption)

    // create, insert, select, drop operations all in a single transaction (Postgres supports transactional DDL)
    val query = for {
      _ <- createTable.transactionally
      _ <- insertPerson(person)
      p <- select
      _ <- dropTable
    } yield p

    // so far, nothing has been issued to the database, we just created a series of actions
    // let's run those actions and get a future of the result
    val result: Future[Option[Person]] = query.run

    result.onComplete {
      case Success(Some(value)) => println(value)
      case Success(None)        => println("not found")
      case Failure(e)           => println(e.getMessage)
    }

    Await.ready(result, Duration.Inf)

    connectionSource.shutdown()
  }

}

```

### Meta Models

In order to use the SQL builder features, we need to define a `metamodels` for our models. For example given the following model:

```scala
case class CompanyId(value: String) extends AnyVal
case class CompanyData(name: Name)
case class Company(id: CompanyId, data: CompanyData)
```

We can define the corresponding `metamodel` as:

```scala
object CompanyModel
    extends Model[Company]
    with HasId[Company, CompanyId]
    with HasData[Company, CompanyData]
    with CanGenerate[CompanyId]
    with CanParse[CompanyId]
    with UUIDGenerator {

    val modelName = "Company"
    val id = property[CompanyId]("id")

    def generate = CompanyId(generateStringId)
    def parse(stringId: String): Try[CompanyId] = parseUUID(stringId)(CompanyId)

    object data extends DataModel[CompanyData] {
      val name = property[Name]("name")
      val properties = Seq(name)
    }

    override val features: Seq[Feature] = Seq(
      sql.primaryKey("company_pk")(id)
    )

  }
```

Once the `metamodel` is defined we can use it in the SQL builder API, for example:

```scala

def insert(data: CompanyData): IO[Company] = {
    val m = CompanyModel
    val id = m.generate
    m.insert(m.id := id.bind, m.data.name := data.name.bind).map(_ => Company(id, data))
}

def find: IO[Seq[Company]] =
    select(CompanyModel.properties:_*).from(CompanyModel).lifted.read[Company]

def find[F <: Operator](filter: F): IO[Seq[Company]] = {
    val m = CompanyModel
    select(m.properties:_*).from(m).where(filter).lifted.read[Company]
}

def findEmployees(id: CompanyId): IO[Seq[Employee]] = {
    val company = CompanyModel
    val employee = EmployeeModel

    select(employee.properties.map('e dot _):_*)
      .from(company as 'c innerJoin employee as 'e on ('c dot company.id === 'e dot employee.ref.company))
      .where('c dot company.id === CompanyId("c1").bind)
      .limit(1.literal)
      .lifted
      .read[Employee]
}


```

Also several DDL and DML methods are available for free, for example:

```scala
import lampetia.example.model._
import lampetia.example.module.ExampleModule

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}


object Auto {

  import ExampleModule.dialect._
  import ExampleModule.sql._

  implicit val connectionSource = ExampleModule.connectionSource
  implicit val executionContext = ExampleModule.executionContext


  def main(args: Array[String]): Unit = {

    val model = CompanyModel

    val actions = for {

      _ <- model.create.transactionally

      _ <- model.insert(model.id := CompanyId("c1").bind, model.data.name := Name("some company").bind)

      v <- model.findOne(model.id === CompanyId("c1").bind)

      _ <- model.update(model.data.name := Name("something").bind)(model.id === CompanyId("c1").bind)

      _ <- model.delete(model.data.name like "%s".literal)

      _ <- model.drop(cascade = false)

    } yield v

    val result = actions.run

    result.onComplete {
      case Success(Some(company)) => println(company)
      case Success(None)          => println("not found")
      case Failure(e)             => println(e.getMessage)
    }

    Await.ready(result, Duration.Inf)

    ExampleModule.terminate()



  }

}

```










