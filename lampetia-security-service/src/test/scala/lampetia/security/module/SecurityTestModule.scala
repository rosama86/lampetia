package lampetia.security.module

import lampetia.sql.ConnectionSourceFactories
import lampetia.sql.dialect.postgresql.Postgresql

/**
 * @author Hossam Karim
 */
object SecurityTestModule extends SecurityModule { self =>

  val dialect = Postgresql

  object configuration extends super.Configuration

  object json extends super.Json

  object sql extends super.Sql {
    val dialect = self.dialect
  }

  def connectionSourceFactories = new ConnectionSourceFactories {}

  lazy val connectionSource = connectionSourceFactories.hikari(
    configuration.pgJdbcDataSourceClassName,
    configuration.pgHost,
    configuration.pgPort,
    configuration.pgDatabase,
    configuration.pgUser,
    configuration.pgPassword,
    configuration.pgMaximumPoolSize,
    configuration.pgLeakDetectionThreshold)

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run(): Unit = {
      configuration.logger.info("[lampetia-security-service] This test configuration should be shutting down now ...")
      configuration.shutdown()
    }
  })
}
