package lampetia.sql.dialect

import lampetia.model.Model
import lampetia.sql.{Ops, ConnectionSourceFactories, JdbcCodec}

/**
 * @author Hossam Karim
 */

package object h2
  extends H2Dsl
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


