package lampetia.sql.dialect

import lampetia.sql.{ConnectionSourceFactories, JdbcCodec}

/**
 * @author Hossam Karim
 */

package object h2
  extends H2Dsl
  with    JdbcCodec
  with    ConnectionSourceFactories
  with    Ops


