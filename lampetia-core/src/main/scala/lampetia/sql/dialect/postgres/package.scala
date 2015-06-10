package lampetia.sql.dialect

import lampetia.sql.{Ops, ConnectionSourceFactories, JdbcCodec}

/**
 * @author Hossam Karim
 */

package object postgres
  extends PgDsl
  with    JdbcCodec
  with    ConnectionSourceFactories
  with    Ops
