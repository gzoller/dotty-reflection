package co.blocke.dotty_reflection
import impl._


case class WithParam[T,U](one:T, two:U)


object RunMe extends App:

  val result = RType.of[WithParam[Int,Boolean]] 
  println(result.show())

  println("done.")
