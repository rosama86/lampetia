package lampetia.security.module

import lampetia.conf.Configuration
import lampetia.security.conf.SecurityConfiguration
import lampetia.security.model.{SecuritySqlFormat, SecurityModel}
import lampetia.store.postgres.PostgresConfiguration

/**
 * @author Hossam Karim
 */

trait SecurityModule
  extends Configuration
     with SecurityConfiguration
     with PostgresConfiguration
     with SecurityModel
     with SecuritySqlFormat

object SecurityModule extends SecurityModule
