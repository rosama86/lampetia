package lampetia.test

import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time._

/**
 * @author Hossam Karim
 */

trait LampetiaFutures {

  def oneMinute: Timeout = Timeout(Span(1, Minutes))

}
