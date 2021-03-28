package monads

object EitherExample extends App {

  val either1: Either[String, Int] = Right(20)
  val either2: Either[String, Int] = Right(10)

  val sum: Either[String, Int] = for {
    a <- either1
    b <- either1
  } yield a + b
  println(sum)

  // cats

  import cats.syntax.either._

  val a = 3.asRight[String]
  println(a)

  def countPositive(nums: List[Int]): Either[String, Int] =
    nums.foldLeft(0.asRight[String])((e, n) =>
      if (n > 0) e.map(cur => cur + 1)
      else "Negative".asLeft
    )

  println(countPositive(List(1,2,3)))
  println(countPositive(List(1,-1,3)))

  val d: Either[String, Int] = Either.fromOption(Some(2), "paranza")

}
