package lampetia.sql.dialect.postgresql

import lampetia.conf.{Configuration, Lifecycle}
import lampetia.model._
import lampetia.model.sql.SqlTypes
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

trait PostgresqlConfiguration extends Lifecycle {
    self: Postgresql with ConnectionSourceFactories with Configuration =>

  def pgJdbcDataSourceClassName: String
  def pgHost: String
  def pgPort: Int
  def pgDatabase: String
  def pgUser: String
  def pgPassword: String
  def pgMaximumPoolSize: Int
  def pgLeakDetectionThreshold: Int

  implicit lazy val context: ConnectionSource = hikari(
    pgJdbcDataSourceClassName,
    pgHost,
    pgPort,
    pgDatabase,
    pgUser,
    pgPassword,
    pgMaximumPoolSize,
    pgLeakDetectionThreshold)

  abstract override def shutdown(): Unit = {
    logger.info(s"[postgres] shutdown sequence: begin")
    context.shutdown()
    logger.info(s"[postgres] shutdown sequence: done")
    super.shutdown()
  }
}


