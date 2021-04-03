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

  println(countPositive(List(1, 2, 3)))
  println(countPositive(List(1, -1, 3)))

  val d: Either[String, Int] = Either.fromOption(Some(2), "paranza")
  println(d)

  val result = for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "Div0".asLeft[Int]
    else (a / b).asRight[String]
  } yield a * b

  println(result)

  // More Either
  sealed trait LoginError extends Product with Serializable

  final case class UserNotFound(username: String) extends LoginError

  final case class PasswordWrong(username: String) extends LoginError

  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)

  type ResultLogin = Either[LoginError, User]

  def handleError(error: LoginError): Unit = error match {
    case UserNotFound(username) => println(s"$username not found")
    case PasswordWrong(username) => println(s"$username password wrong")
    case UnexpectedError => println("Unexpected")
  }

  val result1: ResultLogin = User("Bob", "b0b").asRight
  val result2: ResultLogin = UserNotFound("dave").asLeft

  result1.fold(handleError, println)
  result2.fold(handleError, println)

}
