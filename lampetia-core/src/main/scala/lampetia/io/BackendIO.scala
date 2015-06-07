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
  def pure[A](a: A): Result[A]
  def fail[A](cause: Throwable): Result[A]
  def map[A, B](fa: Result[A])(f: A => B): Result[B]
  def flatMap[A, B](fa: Result[A])(f: A => Result[B]): Result[B]
  def withFilter[A](fa: Result[A])(f: A => Boolean): Result[A]

  trait IO[A] {
    def execute(context: Context): Result[A]
    def run(implicit ec: ExecutionContext, context: Context): Future[A] = self.run(this)
  }

  def run[R](io: IO[R])(implicit ec: ExecutionContext, context: Context): Future[R]

  case class IOPure[A](result: A) extends IO[A] {
    def execute(context: Context): Result[A] = pure(result)
  }

  case class IOFailed[A](exception: Throwable) extends IO[A] {
    def execute(context: Context): Result[A] = fail[A](exception)
  }

  case class IOFlatMap[A, B](fa: IO[A], f: A => IO[B]) extends IO[B] {
    def execute(context: Context): Result[B] =
      flatMap(fa.execute(context))(a => f(a).execute(context))
  }

  case class IOFilter[A, B](fa: IO[A], f: A => Boolean) extends IO[A] {
    def execute(context: Context): Result[A] =
      withFilter(fa.execute(context))(f)
  }


  trait LiftIO[A] extends Any {

    protected def io: IO[A]

    def pure(a: A): IO[A] = IOPure(a)

    def map[B](f: A => B): IO[B] = IOFlatMap(io, (a: A) => IOPure(f(a)))

    def flatMap[B](f: A => IO[B]): IO[B] = IOFlatMap(io, f)

    def withFilter(f: A => Boolean): IO[A] = IOFilter(io, f)

  }
}


