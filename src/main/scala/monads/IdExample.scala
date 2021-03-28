package monads


object IdExample extends App {

  type Id[A] = A

  def pure[A](value: A): Id[A] = value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)

  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def sumSquare[F[_]](a: F[Int], b: F[Int])(implicit m: Monad[F]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  println(sumSquare(5 : Id[Int], 2: Id[Int]))

  import cats.syntax.either._
  val b: Either[String, Int] = 123.asRight[String]
  println(b)
}



