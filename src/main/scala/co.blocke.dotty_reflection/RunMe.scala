package co.blocke.dotty_reflection

trait T5[X, Y] { val thing1: X; val thing2: Y }
trait T10[X, Y] { val x: X; val y: Y }
trait T11[W, Z] { val w: W; val z: Z }
case class TBlah1[A, B](w: A, z: B) extends T11[A, B]
case class TBar7[A, B](thing1: A, thing2: B) extends T5[A, B]
case class TFoo6[A, B, C, D](x: T11[C, T5[D, A]], y: B) extends T10[T11[C, T5[D, A]], B]


trait Level1[T,U] { val t: T; val u: U }
trait Base[A,B] { val a: A; val b: B }
case class L1Class[X,Y]( t: X, u: Y ) extends Level1[X,Y]
case class BaseClass[X,Y]( a: X, b: Y, z: Y ) extends Base[X,Y]

object RunMe extends App:

  // val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
  // val result = RType.inTermsOf[T10[T11[Int, T5[Double, Char]], String]]( inst.getClass )

  val inst: Base[Level1[String,Long],Double] = BaseClass(L1Class("foo",3L), 12.34, 45.67)
  val result = RType.inTermsOf[Base[Level1[String,Long],Double]](inst.getClass)
  println(result.show())

  println("done.")

