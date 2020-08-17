package co.blocke.dotty_reflection
import impl._

object Fruit extends Enumeration {
  val Apple, Orange, Banana = Value
}
object Size extends Enumeration {
  val Small, Medium, Large = Value
}

case class Stuff(a: Map[Fruit.Value,Fruit.Value])

object RunMe extends App:

  val r0 = RType.of[Int]
  val r1 = RType.of[Int]

  // println(r0.show())
  println(r0 == r1)
  println(r0.equals(r1))

  // PROBELM:  Even tho r0 and r1 refer to the same thing, they're non-equal, i.e. 2 different memory addresses == "not the same object"
  println("R0: "+System.identityHashCode(r0))
  println("R1: "+System.identityHashCode(r1))

  println("-----------")

  println("R0: "+r0.hashCode)
  println("R1: "+r1.hashCode)

  println("done.")
