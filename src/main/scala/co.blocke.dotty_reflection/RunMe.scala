package co.blocke.dotty_reflection

/**  This class is a "lab".  Not intended for use with the library.  It's a learning area to see if/how we might move
 *   pieces of this to a macro.
 */

import model._
import impl.Clazzes._
import scala.util.Try

case class Foo[T](a:T)

case class Person(name: String, age: Int)

case class Item()
case class Other()

@main def runme(): Unit =

  // println(Reflector.reflectOn[Foo[Double]])

  println("Running...")
  // println(getPerson("Greg"))

  /*
  try {
    println(getInstance2("b").getClass.getName)
  } catch {
    case _:Exception => println("Expected Failure")
  }
  println(getInstance2("a").getClass.getName)
  println(getInstance2("b").getClass.getName)
  */

  println(impl.getParams[Foo[Boolean]])

  /*
  println(readMe[Foo[Boolean]])

  inline def readMe[T]: String =
    val stuff = getParams[T]
    "Returns: "+stuff
    */
