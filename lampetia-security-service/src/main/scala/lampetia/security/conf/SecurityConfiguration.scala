package lampetia.security.conf

import lampetia.conf.{Configuration, Lifecycle}
import scala.concurrent.ExecutionContext

/**
 * @author Hossam Karim
 */

trait SecurityConfiguration extends Lifecycle { self: Configuration =>

  lazy val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  lazy val schema: String =
    config.getString("lampetia.module.security.postgres.schema")
  lazy val pgJdbcDataSourceClassName: String =
    config.getString("lampetia.module.security.postgres.data-source-class-name")
  lazy val pgHost: String =
    config.getString("lampetia.module.security.postgres.host")
  lazy val pgPort: Int =
    config.getInt("lampetia.module.security.postgres.port")
  lazy val pgDatabase: String =
    config.getString("lampetia.module.security.postgres.database")
  lazy val pgUser: String =
    config.getString("lampetia.module.security.postgres.user")
  lazy val pgPassword: String =
    config.getString("lampetia.module.security.postgres.password")
  lazy val pgMaximumPoolSize: Int =
    config.getInt("lampetia.module.security.postgres.maximum-pool-size")
  lazy val pgLeakDetectionThreshold: Int =
    config.getInt("lampetia.module.security.postgres.leak-detection-threshold")
  

  abstract override def shutdown(): Unit = {
    logger.info(s"[security] shutdown sequence: begin")
    logger.info(s"[secuirty] shutdown sequence: done")
    super.shutdown()
  }

}

