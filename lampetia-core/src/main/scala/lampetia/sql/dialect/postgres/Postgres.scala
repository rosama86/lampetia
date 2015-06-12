package lampetia.sql.dialect.postgres

import lampetia.sql.{SqlCodec, Ops, SqlIO}

/**
 * @author Hossam Karim
 */
trait Postgres
  extends PgDsl
  with    SqlIO
  with    SqlCodec
  with    Ops
