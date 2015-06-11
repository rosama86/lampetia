package lampetia.sql

import java.sql.{PreparedStatement, ResultSet}

import com.zaxxer.hikari.HikariDataSource
import org.joda.time.DateTime

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}


/**
 * @author Hossam Karim
 */


trait JdbcCodec extends SqlCodec { self =>

  type Connection = java.sql.Connection
  type Result[A] = Try[A]

  // monadic result
  object resultM extends super.ResultM {
    def pure[A](a: A): Try[A] = Success(a)
    def fail[A](cause: Throwable): Try[A] = Failure[A](cause)
    def map[A, B](fa: Try[A])(f: (A) => B): Try[B] = fa.map(f)
    def flatMap[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] = fa.flatMap(f)
    def withFilter[A](fa: Try[A])(f: (A) => Boolean): Try[A] = fa.filter(f)
  }




  trait JdbcSqlType[A] {
    def sqlType: Int
  }

  object JdbcSqlType {
    def apply[A](tpe: Int): JdbcSqlType[A] = new JdbcSqlType[A] {
      def sqlType = tpe
    }
  }

  protected class MutableResultSetReader(rs: ResultSet) extends SqlReader {

    protected var index: Int = 1

    def readString: String = {
      val result = rs.getString(index)
      index += 1
      result
    }

    def readBoolean: Boolean = {
      val result = rs.getBoolean(index)
      index += 1
      result
    }

    def readInt: Int = {
      val result = rs.getInt(index)
      index += 1
      result
    }

    def readLong: Long = {
      val result = rs.getLong(index)
      index += 1
      result
    }

    def readDate: DateTime = {
      val result = rs.getDate(index)
      index += 1
      new DateTime(result.getTime)
    }

    def readTimestamp: DateTime = {
      val result = rs.getTimestamp(index)
      index += 1
      new DateTime(result.getTime)
    }
  }

  protected class MutablePreparedStatementWriter(ps: PreparedStatement) extends SqlWriter {

    protected var index: Int = 1

    def writeNull(sqlType: Int): SqlWriter = {
      ps.setNull(index, sqlType)
      index += 1
      this
    }

    def writeString(value: String): SqlWriter = {
      ps.setString(index, value)
      index += 1
      this
    }

    def writeBoolean(value: Boolean): SqlWriter = {
      ps.setBoolean(index, value)
      index += 1
      this
    }

    def writeInt(value: Int): SqlWriter = {
      ps.setInt(index, value)
      index += 1
      this
    }

    def writeLong(value: Long): SqlWriter = {
      ps.setLong(index, value)
      index += 1
      this
    }

    def writeDate(value: DateTime): SqlWriter = {
      ps.setDate(index, new java.sql.Date(value.getMillis))
      index += 1
      this
    }

    def writeTimestamp(value: DateTime): SqlWriter = {
      ps.setTimestamp(index, new java.sql.Timestamp(value.getMillis))
      index += 1
      this
    }

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

    def execute(cm: ConnectionSource): Try[Seq[R]] = {
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

    def execute(cm: ConnectionSource): Try[Int] = {
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

    def execute(cm: ConnectionSource): Try[Seq[R]] = {
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

    def execute(cm: ConnectionSource): Try[Int] = {
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
  private class ConnectionSourceProxy(cm: ConnectionSource) extends ConnectionSource {
    // always return the same connection
    lazy val connection: Connection = cm.connection
    // we are not closing the connection here
    def done(connection: Connection): Unit = ()
    // and never shutting this down
    def shutdown(): Unit = ()
  }

  case class TransactionalSqlIOQ[A](sqlIO: IO[A]) extends TransactionalIO[A] {
    def execute(cm: ConnectionSource): Try[A] = {
      val proxy = new ConnectionSourceProxy(cm)
      val connection = proxy.connection

      if (connection.getAutoCommit)
        connection.setAutoCommit(false)

      sqlIO.execute(proxy) match {
        case success@Success(_) =>
          connection.commit()
          // close through the real connection manager
          cm.done(connection)
          success
        case failure@Failure(_) =>
          connection.rollback()
          // close through the real connection manager
          cm.done(connection)
          failure
      }
    }
  }

  def createTransactionalIO[R](sqlIO: IO[R]): TransactionalIO[R] =
    TransactionalSqlIOQ(sqlIO)



  implicit def consumeOption[A](implicit consume: Consume[A]): Consume[Option[A]] = consume andThen Option.apply
  implicit def produceOption[A](implicit produce: Produce[A], sqlType: JdbcSqlType[A]): Produce[Option[A]] =
    (option: Option[A]) => (sqlWriter: SqlWriter) => option match {
      case Some(value) => produce(value)(sqlWriter)
      case None => sqlWriter.writeNull(sqlType.sqlType)
    } 


  implicit val stringSqlType: JdbcSqlType[String] = JdbcSqlType[String](java.sql.Types.VARCHAR)
  implicit val intSqlType: JdbcSqlType[Int] = JdbcSqlType[Int](java.sql.Types.INTEGER)

  def run[A](io: IO[A])(implicit ec: ExecutionContext, source: ConnectionSource): Future[A] = {
    //val source = connectionSource
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






