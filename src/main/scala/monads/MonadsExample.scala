package monads

object MonadsExample extends App {

  trait Monad[F[_]] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(A => pure(func(A)))
  }

}
