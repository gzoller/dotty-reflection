package co.blocke.dotty_reflection
import impl._


trait Basis[T] {
  val a: Int
  val b: String
  val c: T
}

case class Thingy[T]( a: Int, b: String, c: T) extends Basis[T]


object RunMe extends App:

  println(RType.inTermsOf[Basis[List[Option[Int|Boolean]]]](Class.forName("co.blocke.dotty_reflection.Thingy")))

  // println(RType.of[Basis[Int|String]])


  println("done.")
