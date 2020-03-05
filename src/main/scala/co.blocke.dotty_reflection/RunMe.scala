package co.blocke.dotty_reflection

import model._
import impl.Clazzes._

// object WeekDay extends Enumeration {
//   type WeekDay = Value
//   val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
// }
object WeekDay extends Enumeration {
  type WeekDay = Value
  val Monday = Value(1)
  val Tuesday = Value(2)
  val Wednesday = Value(3)
  val Thursday = Value(4)
  val Friday = Value(5)
  val Saturday = Value(6)
  val Sunday = Value(-3)
}
import WeekDay._

enum Month {
  case Jan, Feb, Mar
}

case class Shell(a: Month)
// case class Shell(a: WeekDay, b: Month)

@main def runme(): Unit =

  // try {
    // println(Reflector.reflectOn[You])

    // println(Reflector.reflectOn[Shell])

    println("-------------")

    val s = ScalaEnumeration("co.blocke.dotty_reflection.WeekDay",Class.forName("co.blocke.dotty_reflection.WeekDay"))
    println(s.values)
    println(s.ordinal("Sunday"))
    println(s.valueOf("Wednesday").isInstanceOf[WeekDay])
    println(s.valueOf(1))

    println("HEY: "+Class.forName("scala.Enumeration$Value"))
    
  // } catch {
  //   case x => //x.printStackTrace()
  // }
