package lampetia.example.module

import lampetia.conf.{Configuration => LampetiaConfiguration}
import lampetia.example.conf.ExampleConfiguration
import lampetia.example.format.{ExampleJsonFormat, ExampleSqlFormat}
import lampetia.example.model.ExampleModel
import lampetia.sql.{ConnectionSourceFactories, JdbcConnectionSource}
import lampetia.sql.dialect.postgresql.{Postgresql, PostgresqlConfiguration}

/**
  * @author Hossam Karim
  */

trait ExampleModule {

  val dialect: Postgresql

  trait Configuration extends LampetiaConfiguration with ExampleConfiguration with PostgresqlConfiguration {
    def close(): Unit = connectionSource.shutdown()
  }

  def configuration: Configuration

  trait Json extends ExampleJsonFormat
  def json: Json


  trait Sql extends ExampleModel with ExampleSqlFormat {
    def schema = configuration.schema
  }

  def sql: Sql

  def connectionSource: JdbcConnectionSource
}

object ExampleModule extends ExampleModule { self =>

  val dialect = Postgresql

  object configuration extends super.Configuration

  object json extends super.Json

  object sql extends super.Sql {
    val dialect = self.dialect
  }

  lazy val connectionSourceFactories = new ConnectionSourceFactories {}

  lazy val connectionSource = connectionSourceFactories.hikariFromDataSourceClass(
    configuration.pgJdbcDataSourceClassName,
    configuration.pgHost,
    configuration.pgPort,
    configuration.pgDatabase,
    configuration.pgUser,
    configuration.pgPassword,
    configuration.pgMaximumPoolSize,
    configuration.pgLeakDetectionThreshold)

  lazy val executionContext = configuration.concurrent.executionContext

}