package co.blocke.dotty_reflection

import munit._
import model._
import PrimitiveType._
import scala.util.{Left,Right}

class Eithers extends munit.FunSuite {

  test("Scala simple Either field") {
    val r = Reflector.reflectOn[BothSides].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.BothSides",
        List(
          ScalaFieldInfo(0,"a",ScalaEitherInfo("scala.util.Either",Scala_Int,Scala_String),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala simple Either field assignment") {
    val r = Reflector.reflectOn[BothSides].asInstanceOf[StaticClassInfo]
    assert(
      r.constructWith[BothSides](List(Right("Foom"))) == BothSides(Right("Foom"))
    )
    assert(
      r.constructWith[BothSides](List(Left(3))) == BothSides(Left(3))
    )
  }

  test("Scala Either with Option") {
    val r = Reflector.reflectOn[BothSidesWithOption].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.BothSidesWithOption",
        List(
          ScalaFieldInfo(0,"a",ScalaEitherInfo("scala.util.Either",Scala_Int,ScalaOptionInfo("scala.Option",Scala_String)),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala Either with Option assignment") {
    val r = Reflector.reflectOn[BothSidesWithOption].asInstanceOf[StaticClassInfo]
    assert(
      r.constructWith[BothSidesWithOption](List(Right(None))) == BothSidesWithOption(Right(None))
    )
    assert(
      r.constructWith[BothSidesWithOption](List(Right(Some("x")))) == BothSidesWithOption(Right(Some("x")))
    )
  }

  test("Scala Either with Union type") {
    val r = Reflector.reflectOn[BothSidesWithUnion].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.BothSidesWithUnion",
        List(
          ScalaFieldInfo(0,"a",ScalaEitherInfo("scala.util.Either",Scala_Int,StaticUnionInfo("__union_type__",Nil,Scala_String,Scala_Boolean)),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala Either with Union type assignment") {
    val r = Reflector.reflectOn[BothSidesWithUnion].asInstanceOf[StaticClassInfo]
    assert(
      r.constructWith[BothSidesWithUnion](List(Right("foo"))) == BothSidesWithUnion(Right("foo"))
    )
    assert(
      r.constructWith[BothSidesWithUnion](List(Right(true))) == BothSidesWithUnion(Right(true))
    )
  }
}