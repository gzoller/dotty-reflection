package co.blocke.dotty_reflection

import java.nio.ByteBuffer

trait Message[M,N]:
  val msg: M
  val n:   N

trait CommandMessage[C,D] extends Message[D,List[C]]:
  val thing: D

case class Command[X,Y,Z](
  stuff: String, 
  one: X, 
  msg: Z, 
  n: List[Option[Y]],
  y: Y,
  thing: Z) extends CommandMessage[Option[Y], Z]  // extends Message[Z, List[Option[Y]]] --> Path for Y should be: (1,0,0)


// trait T5[X, Y] { val thing1: X; val thing2: Y }
// trait T10[X, Y] { val x: X; val y: Y }
// trait T11[W, Z] { val w: W; val z: Z }
// case class TBlah1[A, B](w: A, z: B) extends T11[A, B]
// case class TBar7[A, B](thing1: A, thing2: B) extends T5[A, B]
// case class TFoo6[A, B, C, D](x: T11[C, T5[D, A]], y: B) extends T10[T11[C, T5[D, A]], B]


object RunMe extends App:

  // println(RType.inTermsOf[CommandMessage[Option[String],Boolean]](classOf[Command[_,_,_]]))
  // println(RType.inTermsOf[Message[Boolean, List[Option[String]]]](classOf[Command[_,_,_]]))

  // val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
  // val result = RType.inTermsOf[T10[T11[Int, T5[Double, Char]], String]]( inst.getClass )
  // println(result)

  println("Done.")
