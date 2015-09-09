package lampetia.spray

import lampetia.conf.Configuration

/**
 * @author Hossam Karim
 */

trait SprayConfiguration {

  def configuration: Configuration

  def moduleConfigurationPrefix: String

  def apiPrefix: String =
    configuration.config.getString(s"$moduleConfigurationPrefix.spray.api.prefix")

  def apiVersion: String =
    configuration.config.getString(s"$moduleConfigurationPrefix.spray.api.version")

  def internalApiPrefix: String =
    configuration.config.getString(s"$moduleConfigurationPrefix.spray.internal-api.prefix")

  def internalApiVersion: String =
    configuration.config.getString(s"$moduleConfigurationPrefix.spray.internal-api.version")

  def serviceHost: String =
    configuration.config.getString(s"$moduleConfigurationPrefix.spray.service.host")

  def servicePort: Int =
    configuration.config.getInt(s"$moduleConfigurationPrefix.spray.service.port")


}
