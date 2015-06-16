package lampetia.security.module

/**
 * @author Hossam Karim
 */
object SecurityTestModule extends SecurityModule {

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run(): Unit = {
      logger.info("This test configuration should be shutting down now ...")
      shutdown()
    }
  })
}
