package co.blocke.dotty_reflection
import impl._


case class WithParam[X](x:X)


object RunMe extends App:

  val result = RType.of[Wrapper[Int]] 
  println(result.show())
  println(result.asInstanceOf[info.JavaClassInfo].paramTypes.toList)

  println("done.")
