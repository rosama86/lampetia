package lampetia.sql.dialect.postgresql

import lampetia.conf.{Configuration, Lifecycle}
import lampetia.meta._
import lampetia.model._
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
      case DoubleProperty => "double"
      case LongProperty => "long"
      case StringProperty => "varchar"
      case DateProperty  => "date"
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

trait PostgresqlConfiguration extends Lifecycle { self: Configuration =>

  lazy val pgJdbcDataSourceClassName: String =
    config.getString("lampetia.module.postgres.data-source-class-name")
  lazy val pgHost: String =
    config.getString("lampetia.module.postgres.host")
  lazy val pgPort: Int =
    config.getInt("lampetia.module.postgres.port")
  lazy val pgDatabase: String =
    config.getString("lampetia.module.postgres.database")
  lazy val pgUser: String =
    config.getString("lampetia.module.postgres.user")
  lazy val pgPassword: String =
    config.getString("lampetia.module.postgres.password")
  lazy val pgMaximumPoolSize: Int =
    config.getInt("lampetia.module.postgres.maximum-pool-size")
  lazy val pgLeakDetectionThreshold: Int =
    config.getInt("lampetia.module.postgres.leak-detection-threshold")

  def close(): Unit


  abstract override def shutdown(): Unit = {
    logger.info(s"[postgres] shutdown sequence: begin")
    //context.shutdown()
    close()
    logger.info(s"[postgres] shutdown sequence: done")
    super.shutdown()
  }
}


