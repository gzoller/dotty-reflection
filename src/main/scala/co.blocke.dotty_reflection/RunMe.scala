package co.blocke.dotty_reflection


case class Person[Z](name: String, age: Int, other: Foo[Boolean,Z])
case class Foo[Z,T](it: T, other: Z)

object RunMe extends App:

  // println(Reflector.reflectOn[Person[Int]])

  println(Reflector.reflectOn[Person[String]])