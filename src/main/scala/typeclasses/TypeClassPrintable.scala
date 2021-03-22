package typeclasses

object TypeClassPrintable extends App {

  trait Printable[A] {
    def format(value: A): String
  }

  object PrintableInstances {
    implicit val stringPrintable: Printable[String] = new Printable[String] {
      override def format(value: String): String = value
    }
    implicit val intPrintable: Printable[Int] = new Printable[Int] {
      override def format(value: Int): String = value.toString
    }
  }

  object Printable {
    def format[A](input: A)(implicit printer: Printable[A]): String = printer.format(input)

    def print[A](input: A)(implicit printer: Printable[A]): Unit = println(format(input))
  }

  implicit class PrintableOps[A](value: A) {
    def format(implicit printer: Printable[A]): String = printer.format(value)

    def print(implicit printer: Printable[A]): Unit = println(format(printer))
  }

  import PrintableInstances._

  4.print
  Printable.format(5)
  Printable.print(5)

  case class Cat(name: String)

  implicit val catPrintable: Printable[Cat] = (value: Cat) => value.name

  Cat("pippo").print
}
