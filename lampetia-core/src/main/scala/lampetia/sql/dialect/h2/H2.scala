package lampetia.sql.dialect.h2

import lampetia.sql.{SqlCodec, Ops, SqlIO}

/**
 * @author Hossam Karim
 */
trait H2
  extends H2Dsl
  with    SqlIO
  with    SqlCodec
  with    Ops
