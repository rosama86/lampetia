package lampetia.sql.dialect

import lampetia.model.Model
import lampetia.sql.{ConnectionSourceFactories, JdbcCodec, Ops}

/**
 * @author Hossam Karim
 */

package object postgres
  extends PgDsl
  with    JdbcCodec
  with    ConnectionSourceFactories
  with    Ops {

  implicit class ModelOps[E](val model: Model[E])
    extends AnyVal
    with    ModelSchema[E]
    with    DDL[E]
    with    Find[E]
    with    Insert[E]
    with    Update[E]
    with    Delete[E]
}
