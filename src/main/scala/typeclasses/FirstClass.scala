package typeclasses

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

object FirstClass extends App {
  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString: String = showInt.show(123)
  val stringAsString: String = showString.show("Abs")
  val showInt2 = 123.show
  val showString2 = "abc".show
  println(showInt2)
  import java.util.Date

  implicit val dateShow: Show[Date] = new Show[Date]:
      override def show(t: Date): String = s"${t.getTime}ms"


  new Date().show

  import cats.Eq

  val eqInt = Eq[Int]

  import cats.syntax.eq._

  123 === 123

  case class Cat(name: String)

  implicit val catEquality: Eq[Cat] = Eq.instance[Cat]((cat1, cat2) => cat1.name === cat2.name)
  val cat1 = Cat("a")
  val cat2 = Cat("B")
  cat1 === cat2
  import cats.instances.option._
  Option(cat1) === Option(cat2)
  Option(cat1) === None
}
