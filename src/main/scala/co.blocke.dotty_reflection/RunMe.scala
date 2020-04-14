package co.blocke.dotty_reflection
import info._


trait Foom
trait Thing[A, B] { val a: A; val b: B }
case class AThing[Y, X](a: X, b: Y) extends Thing[X, Y] with Foom

case class Hey[U,T](a: Array[T], b: U)

case class Pet(callsign: String, isOk: Boolean)

// Need to detect Z in Hey declaration--just see U & T right now
case class Person[Z](name: String, age: Hey[Z,Boolean])

object RunMe extends App:

  val i = Reflector.reflectOn[Person[Int]]
  println(i)