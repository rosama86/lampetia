package lampetia.test

import lampetia.model.{Model, sql}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * A port of Slick Coffees example to lampetia
 * @author Hossam Karim
 */
object CoffeeExample extends App {

  import lampetia.sql.dialect.h2.jdbc._
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

  case class Supplier(id: Int, name: String, street: String, city: String, state: String, zip: String)

  object SupplierModel extends Model[Supplier] {
    val modelName = "Suppliers"
    val id = property[Int]("id").set(sql.name("SUP_ID")).set(sql.`type`("integer"))
    val name = property[String]("name")
    val street = property[String]("street")
    val city = property[String]("city")
    val state = property[String]("state")
    val zip = property[String]("zip")
    override val properties = Seq(id, name, street, city, state, zip)
  }

  case class Coffee(id: String, supplierId: Int, price: Double, sales: Int, total: Int)

  object CoffeeModel extends Model[Coffee] {
    val modelName = "Coffees"
    val id = property[String]("id")
    val supplierId = property[Int]("supplierId").set(sql.name("SUP_ID"))
    val price = property[Double]("price").set(sql.`type`("double"))
    val sales = property[Int]("sales").set(sql.`type`("integer"))
    val total = property[Int]("total").set(sql.`type`("integer"))
    override val properties = Seq(id, supplierId, price, sales, total)
    override val features = Seq(
      sql.foreignKey(supplierId)(SupplierModel, SupplierModel.id)
    )
  }

  implicit val consumeSupplier: Consume[Supplier] =
    (consume[Int] and
      consume[String] and
      consume[String] and
      consume[String] and
      consume[String] and
      consume[String])(Supplier)

  implicit val produceSupplier: Produce[Supplier] = a =>
    produce(a.id) andThen
      produce(a.name) andThen
      produce(a.street) andThen
      produce(a.city) andThen
      produce(a.state) andThen
      produce(a.zip)

  implicit val consumeCoffee: Consume[Coffee] =
    (consume[String] and
      consume[Int] and
      consume[Double] and
      consume[Int] and
      consume[Int])(Coffee)

  implicit val produceCoffee: Produce[Coffee] = a =>
    produce(a.id) andThen
      produce(a.supplierId) andThen
      produce(a.price) andThen
      produce(a.sales) andThen
      produce(a.total)

  implicit val consumeCustom: Consume[(Coffee, String, String)] =
    (consume[Coffee] and consume[String] and consume[String])((_, _, _))


  val suppliers = SupplierModel
  val coffees = CoffeeModel

  val setup = IO.sequence(
    suppliers.create,
    coffees.create,
    suppliers += Supplier(101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199"),
    suppliers += Supplier(49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460"),
    suppliers += Supplier(150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966"),
    coffees ++=(
      Coffee("Colombian", 101, 7.99, 0, 0),
      Coffee("French_Roast", 49, 8.99, 0, 0),
      Coffee("Espresso", 150, 9.99, 0, 0),
      Coffee("Colombian_Decaf", 101, 8.99, 0, 0),
      Coffee("French_Roast_Decaf", 49, 9.99, 0, 0))
  )


  run(setup.transactionally)

  run(coffees.find)


  val cols = coffees.properties.map(p => 'c dot p) :+ ('s dot suppliers.name) :+ ('s dot suppliers.city)

  val custom =
    select(cols: _*)
      .from(coffees as 'c innerJoin suppliers as 's on ('c dot coffees.supplierId === 's dot suppliers.id))
      .where(suppliers.name like "Superior%".bind)
      .lifted
      .read[(Coffee, String, String)]

  run(custom)

  context.shutdown()


}
