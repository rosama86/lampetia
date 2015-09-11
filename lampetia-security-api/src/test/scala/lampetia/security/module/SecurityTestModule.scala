package lampetia.security.module

import lampetia.sql.dialect.postgresql.Postgresql

/**
 * @author Hossam Karim
 */
object SecurityTestModule extends SecurityModule {

  val dialect = Postgresql

  object configuration extends super.Configuration

  object json extends super.Json

  object sql extends super.Sql

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run(): Unit = {
      configuration.logger.info("This test configuration should be shutting down now ...")
      configuration.shutdown()
    }
  })
}
