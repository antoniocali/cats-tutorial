package functors

import cats.Functor
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.show._

object FunctorExample extends App {

  def doMath[F[_]](f: F[Int])(implicit functor: Functor[F]): F[Int] =
    f.map(_ + 1)

  val option: Option[Int] = Option(1)
  println(doMath(option).show)

  val list = List(1, 2, 3)
  println(doMath(list).show)

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  sealed trait Tree[+A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Tree.branch(map(left)(f), map(right)(f))
      case Leaf(value) => Tree.leaf(f(value))
    }
  }

  println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_+2))
//  doMath(Tree.branch(Tree.leaf("10"), Tree.leaf("20"))) <- it will fail
}
