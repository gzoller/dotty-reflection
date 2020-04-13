package co.blocke.dotty_reflection
import info._


trait Foom
trait Thing[A, B] { val a: A; val b: B }
case class AThing[Y, X](a: X, b: Y) extends Thing[X, Y] with Foom

case class Hey[T](a: Array[T])

case class Person(name: String, age: Map[String,Int])
case class Pet(callsign: String, isOk: Boolean)

object RunMe extends App:

  // val info = Reflector.reflectOn[GeneralHolder[Map[ Optional[List[String]], Int ]]]
  // println(info)
  
  // println("------------")
  val info = Reflector.reflectOn[Person]
  println(info)