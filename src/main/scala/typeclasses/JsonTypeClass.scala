package typeclasses

import jdk.nashorn.api.scripting.JSObject
import typeclasses.JsonSyntax.JsValueOps

// AST
sealed trait JsValue {
}

case object JsNull extends JsValue {
  override def toString: String = "Null"
}

class JsString(val get: String) extends JsValue {
  override def toString: String = f""""${get}""""
}

class JsNumber(val get: Double) extends JsValue {
  override def toString: String = get.toString
}

class JsObject(val get: Map[String, JsValue]) extends JsValue {
  override def toString: String = get.map { case (key, value) => f""""$key" : $value""" }.mkString("{", ",", "}")
}

class JsArray(val get: List[JsValue]) extends JsValue {
  override def toString: String = get.mkString("[", ",", "]")
}

//Type Class
trait JsonWriter[A] {
  def write(value: A): JsValue
}

object JsValue {
  def toJson[A](value: A)(implicit jsonWriter: JsonWriter[A]): JsValue = jsonWriter.write(value)
}

object JsonWriter {
  implicit val jsonStringWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String) = new JsString(value)
  }

  implicit val jsonStringNumber: JsonWriter[Double] = new JsonWriter[Double] {
    override def write(value: Double): JsValue = new JsNumber(value)
  }
  implicit val jsonStringInt: JsonWriter[Int] = new JsonWriter[Int] {
    override def write(value: Int): JsValue = new JsNumber(value.toDouble)
  }
}

object JsonSyntax {
  implicit class JsValueOps[A](value: A)(implicit jsonWriter: JsonWriter[A]) {
    def toJson: JsValue = jsonWriter.write(value)
  }
}

object JsonTypeClass extends App {
  JsValue.toJson(5)
  5.toJson
  println("hello".toJson)
  println(new JsObject(Map("ciao" -> 5.toJson)))
}
