package lampetia.store.postgres

import lampetia.conf.{Configuration, Lifecycle}
import lampetia.sql.dialect.postgres.jdbc._

/**
 * @author Hossam Karim
 */

trait PostgresConfiguration extends Lifecycle { self: Configuration =>

  def pgJdbcDataSourceClassName: String
  def pgHost: String
  def pgPort: Int
  def pgDatabase: String
  def pgUser: String
  def pgPassword: String
  def pgMaximumPoolSize: Int
  def pgLeakDetectionThreshold: Int

  implicit lazy val context: ConnectionSource = hikari(
      pgJdbcDataSourceClassName,
      pgHost,
      pgPort,
      pgDatabase,
      pgUser,
      pgPassword,
      pgMaximumPoolSize,
      pgLeakDetectionThreshold)

  abstract override def shutdown(): Unit = {
    logger.info(s"[postgres] shutdown sequence: begin")
    context.shutdown()
    logger.info(s"[postgres] shutdown sequence: done")
    super.shutdown()
  }
}
