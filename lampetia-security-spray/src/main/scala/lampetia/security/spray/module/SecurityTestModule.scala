package lampetia.security.spray.module

import lampetia.security.module.SecurityModule

/**
 * @author rhelal
 */
object SecurityTestModule extends SecurityModule {

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run(): Unit = {
      configuration.logger.info("This test configuration should be shutting down now ...")
      configuration.shutdown()
    }
  })

}
