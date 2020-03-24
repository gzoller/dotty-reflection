package co.blocke.dotty_reflection


trait Body
case class FancyBody(message: String) extends Body

case class Envelope[T <: Body, U](id: String, body: T) {
  type Giraffe = T
  type Foo = Int
}

object Runme extends App:

  // println(Reflector.reflectOn[Plain])

  println(Reflector.reflectOn[Envelope[FancyBody,Boolean]])