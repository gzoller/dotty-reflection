package co.blocke.dotty_reflection

import impl._ 
import info._


trait Level0[P]
trait Level1[L]{ val m: Level0[L] }
trait Level2[T,U] extends Level1[U] { val x: Int; val m: Level0[U]; val n: T }
// trait Level2[T,U] { val x: Int; val m: Set[Level0[U]]; val n: List[T] }


trait T10[X] { val x: X }
trait T11[W,T] extends T12[W,T] { val w: W; val t: T }
trait T12[Z,F] { val w: Z; val t: F }
case class TBlah1[A,B](w: A, t: B) extends T11[A,B]
case class TFoo6[C,D](x: T11[C,D]) extends T10[T11[C,D]]

case class Foom[X,Y](val x: Int, m: Level0[Y], n: X) extends Level2[X,Y]
// case class Foom[X,Y](val x: Int, m: Set[Level0[Y]], n: List[X]) extends Level2[X,Y]

object RunMe extends App:

  val c = Class.forName("co.blocke.dotty_reflection.Foom")
  val r = Reflector.reflectOn[Level2[Boolean,Long]]
  val r2 = Reflector.reflectOn[Level1[Int]]
  println(Reflector.reflectOnClassInTermsOf(c, r))

  // val r = Reflector.reflectOn[T10[T12[Int,Boolean]]]
  // val inst = TFoo6(TBlah1(5,true))
  // val result = Reflector.reflectOnClassInTermsOf( inst.getClass, r )

