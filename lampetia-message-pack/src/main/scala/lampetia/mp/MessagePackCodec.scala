package lampetia.mp

import java.io.ByteArrayOutputStream

import lampetia.codec.{Codec, OptionCodecs, PrimitiveCodecs}
import org.joda.time.DateTime
import org.msgpack.core.{MessagePack, MessagePacker, MessageUnpacker}

/**
 * @author Hossam Karim
 */

trait MessagePackCodec extends Codec with PrimitiveCodecs with OptionCodecs {

  type Reader = MessageUnpacker
  type Writer = MessagePacker

  def packer(f: MessagePacker => MessagePacker): Array[Byte] = {
    val out = new ByteArrayOutputStream
    val packer = MessagePack.newDefaultPacker(out)
    f(packer)
    packer.close()
    out.toByteArray
  }

  def unpacker[A](bytes: Array[Byte])(f: MessageUnpacker => A): A = {
    val unpacker = MessagePack.newDefaultUnpacker(bytes)
    val instance = f(unpacker)
    unpacker.close()
    instance
  }

  @inline implicit def consumeSeq[A](implicit consume: Consume[A]): Consume[Seq[A]] =
    unpacker => {
      val length = unpacker.unpackArrayHeader
      (1 to length).map(_ => consume(unpacker))
    }


  @inline implicit def produceSeq[A](implicit produce: Produce[A]): Produce[Seq[A]] =
    seq => packer => {
      packer.packArrayHeader(seq.length)
      seq.foldLeft(packer)( (accPacker, instance) => produce(instance)(accPacker))
    }

  @inline implicit def consumeOption[A](implicit consume: Consume[A]): Consume[Option[A]] =
    unpacker => {
      if (unpacker.hasNext) {
        unpacker.getNextFormat.getValueType match {
          case t if t.isNilType =>
            unpacker.unpackNil()
            None
          case t =>
            val value = consume.apply(unpacker)
            Some(value)
        }
      } else None
    }


  @inline implicit def produceOption[A](implicit produce: Produce[A], codecType: CodecType[A]): Produce[Option[A]] =
    option => packer => {
      option match {
        case Some(a) =>
          produce(a)(packer)
        case None    =>
          packer.packNil()
      }
      packer
    }

  implicit class ImplicitPack[A](val instance: A)(implicit produce: Produce[A]) {
    def pack: Array[Byte] = packer(produce(instance))
  }

  implicit class ImplicitUnpack(val bytes: Array[Byte]) {
    def unpack[A](implicit consume: Consume[A]) = unpacker(bytes)(consume)
  }

  implicit val consumeString: Consume[String] = _.unpackString
  implicit val produceString: Produce[String] = a => p => p.packString(a)
  implicit val consumeInt: Consume[Int] = _.unpackInt
  implicit val produceInt: Produce[Int] = a => p => p.packInt(a)
  implicit val consumeLong: Consume[Long] = _.unpackLong
  implicit val produceLong: Produce[Long] = a => p => p.packLong(a)
  implicit val consumeBoolean: Consume[Boolean] = _.unpackBoolean
  implicit val produceBoolean: Produce[Boolean] = a => p => p.packBoolean(a)
  implicit val consumeDouble: Consume[Double] = _.unpackDouble
  implicit val produceDouble: Produce[Double] = a => p => p.packDouble(a)
  implicit val consumeDateTime: Consume[DateTime] = reader => new DateTime(reader.unpackLong)
  implicit val produceDateTime: Produce[DateTime] = a => p => p.packLong(a.getMillis)

  implicit val intCodecType: CodecType[Int] = CodecType[Int](1)
  implicit val stringCodecType: CodecType[String] = CodecType[String](2)
  implicit val longCodecType: CodecType[Long] = CodecType[Long](3)
  implicit val doubleCodecType: CodecType[Double] = CodecType[Double](4)
  implicit val booleanCodecType: CodecType[Boolean] = CodecType[Boolean](5)
  implicit val dateTimeCodecType: CodecType[DateTime] = CodecType[DateTime](6)
  implicit def defaultCodecType[A]: CodecType[A] = CodecType[A](0)

}
