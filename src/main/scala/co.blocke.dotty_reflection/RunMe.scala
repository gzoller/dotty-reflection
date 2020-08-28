package co.blocke.dotty_reflection
import impl._

case class CClass[X](x:List[X])
class PClass[Y](val y:List[Y])
trait ClassistBaseInv[T,U]{ val t: CClass[T]; val u: PClass[U] }
case class ClassistCInv[A,B](t: CClass[A], u: PClass[B]) extends ClassistBaseInv[A,B]

object RunMe extends App:

  val result = RType.inTermsOf[ClassistBaseInv[Int,Short]](Class.forName("co.blocke.dotty_reflection.ClassistCInv"))
  println(result)

  println("done.")
