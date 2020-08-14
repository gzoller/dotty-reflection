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

  println(r0.show())
  println(r0 == r1)
  println(r0.equals(r1))

  println("done.")
