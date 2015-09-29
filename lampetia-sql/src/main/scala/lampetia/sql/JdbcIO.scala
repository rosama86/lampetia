package lampetia.sql

import java.sql.{ResultSet, PreparedStatement}
import java.util.UUID

import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Failure, Success, Try}

/**
 * @author Hossam Karim
 */

trait JdbcConnectionSource extends ConnectionSource {
  type Connection = java.sql.Connection
}

trait JdbcIO extends SqlIO[JdbcConnectionSource] { codec: JdbcCodec =>

  private val log = LoggerFactory.getLogger("jdbc-io")

  type Result[A] = Try[A]
  type Connection = java.sql.Connection

  // monadic result
  object resultM extends super.ResultM {
    def pure[A](a: A): Try[A] = Success(a)
    def fail[A](cause: Throwable): Try[A] = Failure[A](cause)
    def map[A, B](fa: Try[A])(f: (A) => B): Try[B] = fa.map(f)
    def flatMap[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] = fa.flatMap(f)
    def withFilter[A](fa: Try[A])(f: (A) => Boolean): Try[A] = fa.filter(f)
  }

  type PreparedStatementFactory = (String, Connection) => PreparedStatement

  def preparedStatementFactory(f: SqlWriter => SqlWriter): PreparedStatementFactory = (sql, connection) => {
    val ps = connection.prepareStatement(sql)
    val writer = new MutablePreparedStatementWriter(ps)
    f(writer)
    ps
  }

  type ResultSetReader[A] = (ResultSet) => A

  def resultSetReader[A](f: SqlReader => A): ResultSetReader[A] = (rs) => {
    val reader = new MutableResultSetReader(rs)
    val instance = f(reader)
    instance
  }

  case class ReadPlainSqlQ[R](plainSql: PlainSql, consumer: Consume[R]) extends ReadPlainSql[R] {

    def execute(cm: JdbcConnectionSource): Try[Seq[R]] = {
      var ps: PreparedStatement = null
      var rs: ResultSet = null
      val connection = cm.connection
      try {
        ps = connection.prepareStatement(plainSql.sqlString)
        rs = ps.executeQuery()
        val rsReader = resultSetReader { sqlReader => consumer(sqlReader) }

        val vec = new ListBuffer[R]
        while(rs.next) {
          vec.append(rsReader(rs))
        }
        Success(vec)
        //Success(Stream.continually(rs.next).takeWhile(identity).map(_ => rsReader(rs)).force)

      } catch {
        case e: Throwable => Failure(e)
      } finally {

        if (ps != null) ps.close()
        if (rs != null) rs.close()
        cm.done(connection)
      }
    }

  }


  def createReadPlainSql[R](plainSql: PlainSql, consumer: Consume[R]): ReadPlainSql[R] =
    ReadPlainSqlQ(plainSql, consumer)

  case class WritePlainSqlQ(plainSql: PlainSql) extends WritePlainSql {

    def execute(cm: JdbcConnectionSource): Try[Int] = {
      var ps: PreparedStatement = null
      val connection = cm.connection
      try {
        ps = connection.prepareStatement(plainSql.sqlString)
        Success(ps.executeUpdate())
      } catch {
        case e: Throwable => Failure(e)
      } finally {

        if(ps != null) ps.close()
        cm.done(connection)
      }
    }

  }


  def createWritePlainSql(plainSql: PlainSql): WritePlainSql = WritePlainSqlQ(plainSql)


  case class ReadParameterizedSqlQ[R](parameterizedSql: ParameterizedSql, consumer: Consume[R]) extends ReadParameterizedSql[R] {

    def execute(cm: JdbcConnectionSource): Try[Seq[R]] = {
      var ps: PreparedStatement = null
      var rs: ResultSet = null
      val connection = cm.connection
      try {
        val psFactory = preparedStatementFactory { sqlWriter =>
          parameterizedSql.parameters.foldLeft(sqlWriter) { (acc, parameter) =>
            parameter.producer(parameter.value)(acc)
          }
        }
        ps = psFactory(parameterizedSql.sqlString, connection)
        rs = ps.executeQuery()
        val rsReader = resultSetReader { sqlReader => consumer(sqlReader) }

        val vec = new ListBuffer[R]
        while(rs.next) {
          vec.append(rsReader(rs))
        }
        Success(vec)

        //Success(Stream.continually(rs.next).takeWhile(identity).map(_ => rsReader(rs)).force)
      } catch {
        case e: Throwable => Failure(e)
      } finally {
        if (rs != null) rs.close()
        if (ps != null) ps.close()
        cm.done(connection)
      }

    }

  }


  def createReadParameterizedSql[R](parameterizedSql: ParameterizedSql, consumer: Consume[R]): ReadParameterizedSql[R] =
    ReadParameterizedSqlQ(parameterizedSql, consumer)

  case class WriteParameterizedSqlQ(parameterizedSql: ParameterizedSql) extends WriteParameterizedSql {

    def execute(cm: JdbcConnectionSource): Try[Int] = {
      var ps: PreparedStatement = null
      val connection = cm.connection
      try {
        val psFactory = preparedStatementFactory { sqlWriter =>
          parameterizedSql.parameters.foldLeft(sqlWriter) { (acc, parameter) =>
            parameter.producer(parameter.value)(acc)
          }
        }
        ps = psFactory(parameterizedSql.sqlString, connection)
        Success(ps.executeUpdate())
      } catch {
        case e: Throwable => Failure(e)
      } finally {
        if (ps != null) ps.close()
        cm.done(connection)
      }

    }
  }


  def createWriteParameterizedSql[R](parameterizedSql: ParameterizedSql): WriteParameterizedSql =
    WriteParameterizedSqlQ(parameterizedSql)

  // this proxy will always return the same connection
  private case class ConnectionSourceProxy(connection: JdbcConnectionSource#Connection) extends JdbcConnectionSource {
    val id = UUID.randomUUID.toString
    // always return the same connection
    //lazy val connection: Connection = connectionSource.connection
    // we are not closing the connection here
    def done(connection: Connection): Unit = ()
    // and never shutting this down
    def shutdown(): Unit = ()
  }

  case class TransactionalSqlIOQ[A](sqlIO: IO[A]) extends TransactionalIO[A] {

    def execute(connectionSource: JdbcConnectionSource): Try[A] = {

      log.debug(s"TransactionalSqlIOQ: source = $connectionSource")

      connectionSource match {
        case pxy: ConnectionSourceProxy =>
          log.debug("Existing Proxy received")
          log.debug(s"IO: $sqlIO")
        case pxy =>
          log.debug("No Proxy received")
          log.debug(s"IO: $sqlIO")
      }

      // check if there is a transaction already opened, open one if none is currently opened
      val (proxy, transactionEntryPoint) = connectionSource match {
        case p: ConnectionSourceProxy => (p, false)
        case _                        => (ConnectionSourceProxy(connectionSource.connection), true)
      }

      val connection = proxy.connection

      if (connection.getAutoCommit)
        connection.setAutoCommit(false)

      if (transactionEntryPoint && log.isDebugEnabled) {
        log.debug(s"STARTING TRANSACTION: $connection")
        log.debug(s"Proxy: ${proxy.id}")
      }

      // start the chain reaction
      sqlIO.execute(proxy) match {
        case success@Success(_) =>
          // do not commit unless this call is the transaction entry point
          if (transactionEntryPoint) {
            connection.commit()
            if (log.isDebugEnabled) {
              log.debug(s"COMMIT: $connection")
              log.debug(s"Proxy: ${proxy.id}")
            }
            // close through the real connection manager
            connectionSource.done(connection)
          }
          // no need to close the connection, since this is really a proxy
          success
        case failure@Failure(_) =>
          if(transactionEntryPoint) {
            connection.rollback()
            if (log.isDebugEnabled) {
              log.debug(s"ROLLBACK: $connection")
              log.debug(s"Proxy: ${proxy.id}")
            }
            // close through the real connection manager
            connectionSource.done(connection)
          }
          failure
      }
    }
  }

  def createTransactionalIO[R](sqlIO: IO[R]): TransactionalIO[R] = TransactionalSqlIOQ(sqlIO)

  def run[A](io: IO[A])(implicit ec: ExecutionContext, source: JdbcConnectionSource): Future[A] = {
    log.debug(s"RUN: source = $source")
    Future {
      io.execute(source) match {
        case Success(result) =>
          result
        case Failure(e)      =>
          throw e
      }
    }

  }

}
