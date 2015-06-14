package lampetia.sql.dialect

import lampetia.model.Model
import lampetia.sql._

/**
 * @author Hossam Karim
 */

package object postgres {

  object jdbc extends Postgres with JdbcIO with JdbcCodec with ConnectionSourceFactories {
    implicit class ModelOps[E](val model: Model[E])
      extends AnyVal
      with    ModelSchema[E]
      with    DDL[E]
      with    Find[E]
      with    Insert[E]
      with    Update[E]
      with    Delete[E]
  }


}
