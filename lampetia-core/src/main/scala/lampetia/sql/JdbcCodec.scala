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

  trait JdbcSqlType[A] {
    def sqlType: Int
  }

  object JdbcSqlType {
    def apply[A](tpe: Int): JdbcSqlType[A] = new JdbcSqlType[A] {
      def sqlType = tpe
    }
  }

  protected[sql] class MutableResultSetReader(rs: ResultSet) extends SqlReader {

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

  protected[sql] class MutablePreparedStatementWriter(ps: PreparedStatement) extends SqlWriter {

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

  implicit def consumeOption[A](implicit consume: Consume[A]): Consume[Option[A]] = consume andThen Option.apply
  implicit def produceOption[A](implicit produce: Produce[A], sqlType: JdbcSqlType[A]): Produce[Option[A]] =
    (option: Option[A]) => (sqlWriter: SqlWriter) => option match {
      case Some(value) => produce(value)(sqlWriter)
      case None => sqlWriter.writeNull(sqlType.sqlType)
    } 

  implicit val stringSqlType: JdbcSqlType[String] = JdbcSqlType[String](java.sql.Types.VARCHAR)
  implicit val intSqlType: JdbcSqlType[Int] = JdbcSqlType[Int](java.sql.Types.INTEGER)

}






