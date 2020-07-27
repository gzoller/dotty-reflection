package co.blocke.dotty_reflection
import impl._

import scala.util.Try

trait MapIt[X,Y,S,T]{ val x: Map[X,Option[Y]]; val s: Array[S]; val t: Array[List[T]] }
case class MapItC[A,B,W,U]( x: Map[A,Option[B]], s: Array[W], t: Array[List[U]]) extends MapIt[A,B,W,U]

object RunMe extends App:


  val result = RType.inTermsOf[MapIt[Int,Double,String,Boolean]](Class.forName("co.blocke.dotty_reflection.MapItC"))
  println(result.show())

  println("done.")