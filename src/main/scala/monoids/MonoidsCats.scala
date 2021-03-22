package monoids

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.semigroup._
import cats.syntax.show._

object MonoidsCats extends App {

  println(Monoid[String].combine("Hello", "Pippo").show)

  val a = Option(22)
  val b = Option(23)
  println(Monoid[Option[Int]].combine(a, b).show)
  a

  "Hello" |+| "Pippo" |+| Monoid[String].empty
  println((1 |+| 2).show)

  def add[T](items: List[T])(implicit monoid: Monoid[T]): T = items.fold(Monoid[T].empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit val monoidOrder: Monoid[Order] = new Monoid[Order] {
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

    override def empty: Order = Order(0, 0)
  }
  println(add(List(Order(1, 1), Order(2, 2))))

}
