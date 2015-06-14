package lampetia.sql.dialect

import lampetia.model.Model
import lampetia.sql.{JdbcCodec, ConnectionSourceFactories, JdbcIO}

/**
 * @author Hossam Karim
 */

package object h2 {

  object jdbc extends H2 with JdbcIO with JdbcCodec with ConnectionSourceFactories {
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


