package monads

object MonadsCatsExample extends App {

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatmap
  import cats.instances.option._
  import cats.syntax.applicative._

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    a.flatMap(x => b.map(y => x * x + y * y))
  }

  val a = 1.pure[Option]
  val b = a.map(_ + 1)
  println(sumSquare(a, b))


}
