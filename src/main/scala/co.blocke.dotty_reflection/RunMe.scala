package co.blocke.dotty_reflection

case class Shape(id: Int, parent: Option[Shape])
case class Drawer[T]( id: Int, nextInChain: Option[Drawer[T]], thing: T)

object RunMe extends App:

  println(RType.of[Drawer[Shape]])

  println("done.")

