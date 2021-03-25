package functors

object IntrafunctorExample extends App {

  trait Codec[A] {
    self =>
    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def decode(value: String): B = dec(self.decode(value))

      override def encode(value: B): String = self.encode(enc(value))
    }

  }

  def encode[A](value: A)(implicit codec: Codec[A]): String = codec.encode(value)

  def decode[A](value: String)(implicit codec: Codec[A]): A = codec.decode(value)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value

    override def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)

  final case class Box[A](value: A)

  implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] = new Codec[Box[A]] {
    override def encode(box: Box[A]): String = codec.encode(box.value)

    override def decode(value: String): Box[A] = Box(codec.decode(value))
  }

  //  implicit def boxCodec2[A](implicit codec: Codec[A]): Codec[Box[A]] = codec.imap(Box(_), _.value)

  println(encode(5))
  println(encode(Box(4)))
  println(decode("5")(boxCodec(intCodec)))
  println(decode("5")(stringCodec))
  println(decode[Box[Int]]("5"))

  // Cats

  import cats.Monoid
  import cats.instances.string._
  import cats.syntax.invariant._
  implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap[Symbol]((elem: String) => Symbol.apply(elem))((elem: Symbol) => elem.name)

}
