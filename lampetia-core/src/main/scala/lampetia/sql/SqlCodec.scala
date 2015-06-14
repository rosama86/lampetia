package lampetia.sql

import lampetia.codec.{PrimitiveCodecs, Codec}
import org.joda.time.DateTime

//import scala.language.{higherKinds, implicitConversions}

/**
 * @author Hossam Karim
 */

trait SqlReader {
  def readString: String
  def readBoolean: Boolean
  def readInt: Int
  def readLong: Long
  def readDouble: Double
  def readDate: DateTime
  def readTimestamp: DateTime
}

trait SqlWriter {
  def writeNull(sqlType: Int): SqlWriter
  def writeString(value: String): SqlWriter
  def writeBoolean(value: Boolean): SqlWriter
  def writeInt(value: Int): SqlWriter
  def writeLong(value: Long): SqlWriter
  def writeDouble(value: Double): SqlWriter
  def writeDate(value: DateTime): SqlWriter
  def writeTimestamp(value: DateTime): SqlWriter
}

trait SqlCodec extends Codec with PrimitiveCodecs {

  type Reader = SqlReader
  type Writer = SqlWriter

  implicit val consumeString: Consume[String] = _.readString
  implicit val produceString: Produce[String] = a => p => p.writeString(a)

  implicit val consumeBoolean: Consume[Boolean] = _.readBoolean
  implicit val produceBoolean: Produce[Boolean] = a => p => p.writeBoolean(a)

  implicit val consumeInt: Consume[Int] = _.readInt
  implicit val produceInt: Produce[Int] = a => p => p.writeInt(a)

  implicit val consumeLong: Consume[Long] = _.readLong
  implicit val produceLong: Produce[Long] = a => p => p.writeLong(a)

  implicit val consumeDouble: Consume[Double] = _.readDouble
  implicit val produceDouble: Produce[Double] = a => p => p.writeDouble(a)

}

