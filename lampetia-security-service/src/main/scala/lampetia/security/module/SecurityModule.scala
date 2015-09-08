package lampetia.security.module

import lampetia.conf.{Configuration => LampetiaConfiguration}
import lampetia.security.format.{SecurityJsonFormat, SecuritySqlFormat}
import lampetia.sql.dialect.postgresql.{PostgresqlConfiguration, Postgresql}
import lampetia.security.conf.SecurityConfiguration
import lampetia.security.model.SecurityModel

/**
 * @author Hossam Karim
 */

trait SecurityModule {

  val dialect: Postgresql

  trait Configuration extends LampetiaConfiguration with SecurityConfiguration with PostgresqlConfiguration {
    def close(): Unit = sql.context.shutdown()
  }
  def configuration: Configuration

  trait Json extends SecurityJsonFormat
  def json: Json

  trait Sql extends SecurityModel with SecuritySqlFormat {
    def schema = configuration.schema
    implicit lazy val context: dialect.ConnectionSource = dialect.hikari(
      configuration.pgJdbcDataSourceClassName,
      configuration.pgHost,
      configuration.pgPort,
      configuration.pgDatabase,
      configuration.pgUser,
      configuration.pgPassword,
      configuration.pgMaximumPoolSize,
      configuration.pgLeakDetectionThreshold)
  }
  def sql: Sql
}

object SecurityModule extends SecurityModule {

  val dialect = Postgresql

  object configuration extends super.Configuration

  object json extends super.Json

  object sql extends super.Sql

}
