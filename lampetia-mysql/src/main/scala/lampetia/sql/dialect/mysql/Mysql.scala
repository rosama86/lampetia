package lampetia.sql.dialect.mysql

import lampetia.conf.{Configuration, Lifecycle}
import lampetia.meta._
import lampetia.model._
import lampetia.meta.feature.sql.SqlTypes
import lampetia.sql._
import language.implicitConversions

/**
 * Created by rhelal on 8/11/15.
 */
trait Mysql extends MysqlDsl
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
      case DoubleProperty => "double"
      case LongProperty => "bigint"
      case StringProperty => "varchar(255)"
      case DateProperty  => "date"
      case DefaultProperty => "varchar(255)"
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

trait MysqlConfiguration extends Lifecycle { self: Configuration =>

  def mysqlConfigurationKey = "lampetia.module.mysql"

  lazy val mysqlJdbcDataSourceClassName: String =
    config.getString(s"$mysqlConfigurationKey.data-source-class-name")
  lazy val mysqlHost: String =
    config.getString(s"$mysqlConfigurationKey.host")
  lazy val mysqlPort: Int =
    config.getInt(s"$mysqlConfigurationKey.port")
  lazy val mysqlDatabase: String =
    config.getString(s"$mysqlConfigurationKey.database")
  lazy val mysqlUser: String =
    config.getString(s"$mysqlConfigurationKey.user")
  lazy val mysqlPassword: String =
    config.getString(s"$mysqlConfigurationKey.password")
  lazy val mysqlMaximumPoolSize: Int =
    config.getInt(s"$mysqlConfigurationKey.maximum-pool-size")
  lazy val mysqlLeakDetectionThreshold: Int =
    config.getInt(s"$mysqlConfigurationKey.leak-detection-threshold")

  def close(): Unit


  abstract override def shutdown(): Unit = {
    logger.info(s"[mysql] shutdown sequence: begin")
    //context.shutdown()
    close()
    logger.info(s"[mysql] shutdown sequence: done")
    super.shutdown()
  }
}