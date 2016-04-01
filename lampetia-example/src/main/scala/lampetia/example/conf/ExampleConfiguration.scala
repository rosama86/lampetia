package lampetia.example.conf

import lampetia.conf.{Configuration, Lifecycle}

/**
  * @author Hossam Karim
  */
trait ExampleConfiguration extends Lifecycle { self: Configuration =>

  lazy val schema: String =
    config.getString("example.module.postgresql.schema")


  abstract override def shutdown(): Unit = {
    super.shutdown()
  }

}