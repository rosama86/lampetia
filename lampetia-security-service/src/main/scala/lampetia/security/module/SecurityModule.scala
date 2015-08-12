package lampetia.security.module

import lampetia.conf.Configuration
import lampetia.security.format.{SecurityJsonFormat, SecuritySqlFormat}
import lampetia.sql.dialect.postgresql.{PostgresqlConfiguration, Postgresql}
import lampetia.security.conf.SecurityConfiguration
import lampetia.security.model.SecurityModel

/**
 * @author Hossam Karim
 */

trait SecurityModule {

  object json extends SecurityJsonFormat

  object sql extends Postgresql with SecurityModel with SecuritySqlFormat {
    val schema = configuration.schema
    implicit lazy val context: sql.ConnectionSource = sql.hikari(
      configuration.pgJdbcDataSourceClassName,
      configuration.pgHost,
      configuration.pgPort,
      configuration.pgDatabase,
      configuration.pgUser,
      configuration.pgPassword,
      configuration.pgMaximumPoolSize,
      configuration.pgLeakDetectionThreshold)
  }

  object configuration
    extends Configuration
      with SecurityConfiguration
      with PostgresqlConfiguration {
    def close(): Unit = sql.context.shutdown()
  }



}

object SecurityModule extends SecurityModule
