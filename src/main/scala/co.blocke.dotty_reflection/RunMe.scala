package co.blocke.dotty_reflection
import impl._

import scala.util.Try

case class Foom[X](x:List[X])
// trait Foom[X]{ val x: X }
trait SomeBase[T]{ val t: Foom[T] }
case class SomeThing[A](t: Foom[A]) extends SomeBase[A]



object RunMe extends App:


  val result = RType.inTermsOf[SomeBase[Int]](Class.forName("co.blocke.dotty_reflection.SomeThing"))
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