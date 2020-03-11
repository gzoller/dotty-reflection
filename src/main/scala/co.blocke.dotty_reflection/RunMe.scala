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


@main def runme(): Unit =

  // println(Reflector.reflectOn[Foo[Int]])

  println("Running...")
  // println(getPerson("Greg"))

  println(getInstance[Item]("co.blocke.dotty_reflection.Item").getClass.getName)