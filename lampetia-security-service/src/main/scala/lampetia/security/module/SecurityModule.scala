package lampetia.security.module

import lampetia.conf.Configuration
import lampetia.security.format.{SecurityJsonFormat, SecuritySqlFormat}
import lampetia.sql.dialect.postgresql.{PostgresqlConfiguration, Postgresql}
import lampetia.security.conf.SecurityConfiguration
import lampetia.security.model.SecurityModel

/**
 * @author Hossam Karim
 */

trait SecurityModule
  extends Configuration
    with SecurityConfiguration
    with Postgresql
    with PostgresqlConfiguration
    with SecurityModel
    with SecurityJsonFormat
    with SecuritySqlFormat

object SecurityModule extends SecurityModule
