package monoids

object MonoidsExample extends App {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoids[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoids {
    def apply[A](implicit monoid: Monoids[A]): Monoids[A] = monoid
  }

  given booleanAndMonoid: Monoids[Boolean] = new Monoids[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y

    override def empty: Boolean = true

  }
  given booleanOrMonoid: Monoids[Boolean] = new Monoids[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
    override def empty: Boolean = false
  }

  Monoids[Boolean](booleanAndMonoid).combine(true, true)

  given setUnionMonoid[A]: Monoids[Set[A]] with {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
    override def empty: Set[A] = Set.empty
  }

  Monoids[Set[String]].combine(Set("A"), Set("B"))

}
