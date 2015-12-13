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
  def readDateOption: Option[DateTime]
  def readTimestamp: DateTime
  def readTimestampOption: Option[DateTime]
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

trait SqlCodec extends Codec with PrimitiveCodecs /*with OptionCodecs*/ {

  type Reader = SqlReader
  type Writer = SqlWriter

  implicit val consumeString: Consume[String] = _.readString
  implicit val consumeStringOption: Consume[Option[String]] = consumeString.fmap(Option.apply)
  implicit val produceString: Produce[String] = a => p => p.writeString(a)
  implicit def produceStringOption: Produce[Option[String]]

  implicit val consumeBoolean: Consume[Boolean] = _.readBoolean
  implicit val produceBoolean: Produce[Boolean] = a => p => p.writeBoolean(a)

  implicit val consumeInt: Consume[Int] = _.readInt
  implicit val consumeIntOption: Consume[Option[Int]] = consumeInt.fmap(Option.apply)
  implicit val produceInt: Produce[Int] = a => p => p.writeInt(a)

  implicit val consumeLong: Consume[Long] = _.readLong
  implicit val produceLong: Produce[Long] = a => p => p.writeLong(a)

  implicit val consumeDouble: Consume[Double] = _.readDouble
  implicit val produceDouble: Produce[Double] = a => p => p.writeDouble(a)

  implicit val consumeDateTime: Consume[DateTime] = _.readTimestamp
  implicit val consumeDateTimeOption: Consume[Option[DateTime]] = _.readDateOption
  implicit val produceDateTime: Produce[DateTime] = a => p => p.writeTimestamp(a)

}

