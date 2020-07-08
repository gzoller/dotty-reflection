package co.blocke.dotty_reflection

case class Person(name:String, age:Int)
case class Pet[T](name: String, id: T)

object RunMe extends App:

  // println(RType.of[Person])

  println(RType.of[Pet[Array[List[Int]]]])