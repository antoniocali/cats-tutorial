package monads

import cats.MonadError
import cats.instances.either._

object MonadErrorExample extends App {
  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  val success: ErrorOr[Int] = monadError.pure(5)
  println(success)
  val failure: ErrorOr[Nothing] = monadError.raiseError("Pippo")
  println(failure)

  val succeed = monadError.handleErrorWith(failure) {
    case "Pippo" => monadError.pure(4)
    case _ => monadError.raiseError("Nope")
  }
  println(succeed)

  val ensured: ErrorOr[Int] = monadError.ensure(success)("Erorr")(_ > 6)
  println(ensured)

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  import cats.syntax.monadError._

  val failure2 = "error2".raiseError[ErrorOr, Int]
  val succeed2 = 5.pure[ErrorOr]
  println(failure2)
  println(succeed2)

  val results2 = failure2.handleErrorWith {
    case "error" => 54.pure
    case _ => ("hello").raiseError[ErrorOr, Int]
  }
  println(results2)
  val result3 = succeed2.ensure("Number low")(_ < 4)
  println(result3)

  import scala.util.Try
  import cats.instances.try_._

  val exception = new RuntimeException("LOL")
  println(exception.raiseError[Try, Int])

  //exercise

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    me.ensure(age.pure[F])(new IllegalArgumentException)(_ >= 18)

  println(validateAdult[Try](18))
  println(validateAdult[Try](8))

  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-1))
}
