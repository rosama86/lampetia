package lampetia.io

import scala.language.higherKinds
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Hossam Karim
 */

trait BackendIO { self =>

  // the result type of all io actions
  type Result[A]
  // the input type for all io actions
  type Context

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
    def execute(context: Context): Result[A]
    def run(implicit ec: ExecutionContext, context: Context): Future[A] = self.run(this)
  }

  def run[R](io: IO[R])(implicit ec: ExecutionContext, context: Context): Future[R]

  protected case class IOPure[A](result: A) extends IO[A] {
    def execute(context: Context): Result[A] = resultM.pure(result)
  }

  protected case class IOFailed[A](exception: Throwable) extends IO[A] {
    def execute(context: Context): Result[A] = resultM.fail[A](exception)
  }

  protected case class IOFlatMap[A, B](fa: IO[A], f: A => IO[B]) extends IO[B] {
    def execute(context: Context): Result[B] =
      resultM.flatMap(fa.execute(context))(a => f(a).execute(context))
  }

  protected case class IOFilter[A, B](fa: IO[A], f: A => Boolean) extends IO[A] {
    def execute(context: Context): Result[A] =
      resultM.withFilter(fa.execute(context))(f)
  }


  def pureIO[A](a: A): IO[A] = IOPure(a)
  def failedIO[A](cause: Throwable): IO[A] = IOFailed[A](cause)

  trait LiftIO[A] extends Any {

    protected def io: IO[A]

    def map[B](f: A => B): IO[B] = IOFlatMap(io, (a: A) => IOPure(f(a)))

    def flatMap[B](f: A => IO[B]): IO[B] = IOFlatMap(io, f)

    def withFilter(f: A => Boolean): IO[A] = IOFilter(io, f)

  }
}


