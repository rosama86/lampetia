package lampetia.codec

import org.joda.time.DateTime

import scala.language.{higherKinds, implicitConversions}
import play.api.libs.functional.syntax._
import play.api.libs.functional._

/**
 * @author Hossam Karim
 */

trait Codec {

  type Reader
  type Writer

  type Consume[+A] = Reader => A
  type Produce[-A] = A => Writer => Writer

  trait CodecType[A] {
    def typeCode: Int
  }

  object CodecType {
    def apply[A](tpe: Int): CodecType[A] = new CodecType[A] {
      def typeCode = tpe
    }
  }

  implicit object ConsumeFunctor extends Functor[Consume] {
    def fmap[A, B](m: Consume[A], f: (A) => B): Consume[B] = u => f(m(u))
  }

  implicit object ConsumeFunctionalCanBuild extends FunctionalCanBuild[Consume] {
    def apply[A, B](ma: Consume[A], mb: Consume[B]): Consume[A ~ B] = u => new ~(ma(u), mb(u))
  }

  implicit def functionalBuilderOps[A](c: Consume[A]): FunctionalBuilderOps[Consume, A] =
    toFunctionalBuilderOps[Consume, A](c)(ConsumeFunctionalCanBuild)

  implicit def functorOps[A](c: Consume[A]): FunctorOps[Consume, A] =
    toFunctorOps[Consume, A](c)(ConsumeFunctor)

  def consume[A](implicit consume: Consume[A]): Reader => A = consume

  def produce[A](a: A)(implicit produce: Produce[A]): Writer => Writer = p => produce(a)(p)

}

trait PrimitiveCodecs { self: Codec =>

  implicit def stringCodecType: CodecType[String]
  implicit def consumeString: Consume[String]
  implicit def produceString: Produce[String]

  implicit def consumeBoolean: Consume[Boolean]
  implicit def produceBoolean: Produce[Boolean]

  implicit def intCodecType: CodecType[Int]
  implicit def consumeInt: Consume[Int]
  implicit def produceInt: Produce[Int]

  implicit def consumeLong: Consume[Long]
  implicit def produceLong: Produce[Long]

  implicit def consumeDouble: Consume[Double]
  implicit def produceDouble: Produce[Double]

  implicit def consumeDateTime: Consume[DateTime]
  implicit def produceDateTime: Produce[DateTime]
}

trait OptionCodecs { self: Codec =>
  implicit def consumeOption[A](implicit consume: Consume[A]): Consume[Option[A]]
  implicit def produceOption[A](implicit produce: Produce[A], codecType: CodecType[A]): Produce[Option[A]]
}
