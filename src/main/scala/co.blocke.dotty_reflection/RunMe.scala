package co.blocke.dotty_reflection
import infos._


trait Foom
trait Thing[A, B] { val a: A; val b: B }
case class AThing[Y, X](a: X, b: Y) extends Thing[X, Y] with Foom


object RunMe extends App:

  // val info = Reflector.reflectOn[GeneralHolder[Map[ Optional[List[String]], Int ]]]
  // println(info)
  
  val info = Reflector.reflectOn[AThing[Int,String]]
  println(info)
