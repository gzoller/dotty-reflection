package co.blocke.dotty_reflection

import munit._
import infos._
import PrimitiveType._
import scala.util.{Left,Right}

class Eithers extends munit.FunSuite {

  test("Scala simple Either field") {
    val r = Reflector.reflectOn[BothSides].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.BothSides",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",EitherInfo("scala.util.Either",_,Scala_Int,Scala_String),_,_,None,false)
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
    val r = Reflector.reflectOn[BothSides].asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[BothSides](List(Right("Foom"))) == BothSides(Right("Foom"))
    )
    assert(
      r.constructWith[BothSides](List(Left(3))) == BothSides(Left(3))
    )
  }

  test("Scala Either with Option") {
    val r = Reflector.reflectOn[BothSidesWithOption].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.BothSidesWithOption",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",EitherInfo("scala.util.Either",_,Scala_Int,ScalaOptionInfo("scala.Option",_,Scala_String)),_,_,None,false)
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
    val r = Reflector.reflectOn[BothSidesWithOption].asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[BothSidesWithOption](List(Right(None))) == BothSidesWithOption(Right(None))
    )
    assert(
      r.constructWith[BothSidesWithOption](List(Right(Some("x")))) == BothSidesWithOption(Right(Some("x")))
    )
  }

  test("Scala Either with Union type") {
    val r = Reflector.reflectOn[BothSidesWithUnion].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.BothSidesWithUnion",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",EitherInfo("scala.util.Either",_,Scala_Int,UnionInfo(Reflector.UNION_CLASS,Scala_String,Scala_Boolean)),_,_,None,false)
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
    val r = Reflector.reflectOn[BothSidesWithUnion].asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[BothSidesWithUnion](List(Right("foo"))) == BothSidesWithUnion(Right("foo"))
    )
    assert(
      r.constructWith[BothSidesWithUnion](List(Right(true))) == BothSidesWithUnion(Right(true))
    )
  }
}