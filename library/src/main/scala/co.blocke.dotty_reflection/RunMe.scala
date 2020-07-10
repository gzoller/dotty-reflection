package co.blocke.dotty_reflection

case class Person(name:String, age:Int)
case class Pet[T](name: String, id: T)

trait Movable[X,Y]{ val x: X; val y: Y }

object RunMe extends App:

  // println(RType.of[Person])

  // println(RType.of[Pet[Array[List[Int]]]])

  // println(RType.of[Movable[Int,Boolean]])

  // println(RType.inTermsOf[Item[Float, Boolean]]("co.blocke.dotty_reflection.Thing"))

  // println(RType.of[Message[Boolean,FieldCommand]])

  println(RType.of[IntersectionHolder])

