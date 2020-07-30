package co.blocke.dotty_reflection
import impl._

import scala.util.Try

case class Foom[X](x:List[X])
class Foom2[Y](val y:List[Y])
trait SomeBase[T,U]{ val t: Foom[T]; val u: Foom2[U] }
case class SomeThing[A,B](t: Foom[A], u: Foom2[B]) extends SomeBase[A,B]





object RunMe extends App:


  val result = RType.inTermsOf[SomeBase[Int,Short]](Class.forName("co.blocke.dotty_reflection.SomeThing"))
  println(result.show())

  println("done.")

  /*
  Map X -> A

  TypeBounds(
    HKTypeLambda(
      List(A), 
      List(
        TypeBounds(
          TypeRef(
            ThisType(TypeRef(NoPrefix,module class scala)),class Nothing),
            TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any)
          )
        ), 
        AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class immutable)),class List),List(TypeParamRef(A)))
      ),HKTypeLambda(List(A), List(TypeBounds(TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Nothing),TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any))), AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class immutable)),class List),List(TypeParamRef(A)))))
  */