package lampetia.io

import scala.language.higherKinds
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Hossam Karim
 */

trait BackendIO[C] { self =>

  // the result type of all io actions
  type Result[A]

  // monadic result
  trait ResultM {
    def pure[A](a: A): Result[A]
    def fail[A](cause: Throwable): Result[A]
    def map[A, B](fa: Result[A])(f: A => B): Result[B]
    def flatMap[A, B](fa: Result[A])(f: A => Result[B]): Result[B]
    def withFilter[A](fa: Result[A])(f: A => Boolean): Result[A]
  }
  
  def resultM: ResultM

  trait IO[A] {
    def execute(context: C): Result[A]
    def run(implicit ec: ExecutionContext, context: C): Future[A] = self.run(this)
  }
  object IO {
    def pure[A](a: A): IO[A] = IOPure(a)
    def failed[A](cause: Throwable): IO[A] = IOFailed[A](cause)
    def seq[A](col: Seq[IO[A]]): IO[Seq[A]] = IOSeq(col)
    def sequence[A](col: IO[A]*): IO[Seq[A]] = IOSeq(col)
    def ioOption[A](option: Option[IO[A]]): IO[Option[A]] = IOOption(option)
  }

  def run[R](io: IO[R])(implicit ec: ExecutionContext, context: C): Future[R]

  protected case class IOPure[A](result: A) extends IO[A] {
    def execute(context: C): Result[A] = resultM.pure(result)
  }

  protected case class IOFailed[A](exception: Throwable) extends IO[A] {
    def execute(context: C): Result[A] = resultM.fail[A](exception)
  }

  protected case class IOFlatMap[A, B](fa: IO[A], f: A => IO[B]) extends IO[B] {
    def execute(context: C): Result[B] =
      resultM.flatMap(fa.execute(context))(a => f(a).execute(context))
  }

  protected case class IOSeq[A](seq: Seq[IO[A]]) extends IO[Seq[A]] {
    def execute(context: C): Result[Seq[A]] =
      seq.foldLeft(resultM.pure(Seq.empty[A])) { (rseq, io) =>
        resultM.flatMap(rseq)(seq => resultM.map(io.execute(context))(a => seq :+ a))
      }
  }

  protected case class IOOption[A](option: Option[IO[A]]) extends IO[Option[A]] {
    def execute(context: C): Result[Option[A]] = option match {
      case Some(io) => resultM.map(io.execute(context))(Some(_))
      case None     => resultM.pure(Option.empty[A])
    }
  }

  protected case class IOFilter[A, B](fa: IO[A], f: A => Boolean) extends IO[A] {
    def execute(context: C): Result[A] =
      resultM.withFilter(fa.execute(context))(f)
  }



  trait LiftIO[A] extends Any {

    protected def io: IO[A]

    def map[B](f: A => B): IO[B] = IOFlatMap(io, (a: A) => IOPure(f(a)))

    def flatMap[B](f: A => IO[B]): IO[B] = IOFlatMap(io, f)

    def withFilter(f: A => Boolean): IO[A] = IOFilter(io, f)

  }
}


