package lampetia.sql

/**
 * @author Hossam Karim
 */

trait Dialect extends SqlCodec with SqlIO with JdbcCodec with JdbcIO with ConnectionSourceFactories
