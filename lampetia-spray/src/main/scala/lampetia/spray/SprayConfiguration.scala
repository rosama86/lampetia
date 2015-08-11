package lampetia.spray

import lampetia.conf.{Lifecycle, Configuration}


/**
 * @author Hossam Karim
 */

trait SprayConfiguration extends Lifecycle { this: Configuration =>

  def apiPrefix: String
  def serviceHost: String
  def servicePort: Int


  abstract override def shutdown(): Unit = {
    logger.info(s"[spray] shutdown sequence: begin")
    logger.info(s"[spray] shutdown sequence: done")
    super.shutdown()
  }

}
