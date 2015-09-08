package lampetia.sql.dialect.postgresql

import lampetia.conf.{Configuration, Lifecycle}
import lampetia.meta._
import lampetia.meta.feature.sql.SqlTypes
import lampetia.sql._
import language.implicitConversions

/**
 * @author Hossam Karim
 */
trait Postgresql
  extends PostgresqlDsl
  with    SqlCodec
  with    SqlIO
  with    JdbcCodec
  with    JdbcIO
  with    ConnectionSourceFactories
  with    Ops {

  implicit val sqlTypes: SqlTypes = new SqlTypes {

    def name(propertyType: PropertyType[_]): String = propertyType match {
      case IntProperty => "integer"
      case FloatProperty => "float"
      case DoubleProperty => "float"
      case LongProperty => "long"
      case StringProperty => "varchar"
      case DateProperty  => "timestamp"
      case DefaultProperty => "text"
    }
  }

  trait ModelOps[E]
    extends ModelSchema[E]
    with    DDL[E]
    with    Find[E]
    with    Insert[E]
    with    Update[E]
    with    Delete[E]

  class ModelOpsEx[E](val model: Model[E]) extends ModelOps[E]

  implicit def modelOps[E](model: Model[E]): ModelOps[E] = new ModelOpsEx[E](model)



}

object Postgresql extends Postgresql

trait PostgresqlConfiguration extends Lifecycle { self: Configuration =>

  def postgresqlConfigurationKey = "lampetia.module.postgres"

  lazy val pgJdbcDataSourceClassName: String =
    config.getString(s"$postgresqlConfigurationKey.data-source-class-name")
  lazy val pgHost: String =
    config.getString(s"$postgresqlConfigurationKey.host")
  lazy val pgPort: Int =
    config.getInt(s"$postgresqlConfigurationKey.port")
  lazy val pgDatabase: String =
    config.getString(s"$postgresqlConfigurationKey.database")
  lazy val pgUser: String =
    config.getString(s"$postgresqlConfigurationKey.user")
  lazy val pgPassword: String =
    config.getString(s"$postgresqlConfigurationKey.password")
  lazy val pgMaximumPoolSize: Int =
    config.getInt(s"$postgresqlConfigurationKey.maximum-pool-size")
  lazy val pgLeakDetectionThreshold: Int =
    config.getInt(s"$postgresqlConfigurationKey.leak-detection-threshold")

  def close(): Unit


  abstract override def shutdown(): Unit = {
    logger.info(s"[postgres] shutdown sequence: begin")
    //context.shutdown()
    close()
    logger.info(s"[postgres] shutdown sequence: done")
    super.shutdown()
  }
}


