package co.blocke.dotty_reflection

import munit._
import info._
import PrimitiveType._
import scala.util.{Left,Right}

class Eithers extends munit.FunSuite:

  test("Scala simple Either field") {
    val result = Reflector.reflectOn[BothSides]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.BothSides):
    |   fields:
    |      (0) a: Either:
    |         left--scala.Int
    |         right--java.lang.String""".stripMargin)
  }

  test("Scala simple Either field assignment") {
    val r = Reflector.reflectOn[BothSides].asInstanceOf[ScalaCaseClassInfo]
    assert(
      r.constructWith[BothSides](List(Right("Foom"))) == BothSides(Right("Foom"))
    )
    assert(
      r.constructWith[BothSides](List(Left(3))) == BothSides(Left(3))
    )
  }

  test("Scala Either with Option") {
    val result = Reflector.reflectOn[BothSidesWithOption]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.BothSidesWithOption):
    |   fields:
    |      (0) a: Either:
    |         left--scala.Int
    |         right--Option of java.lang.String""".stripMargin)
  }

  test("Scala Either with Option assignment") {
    val r = Reflector.reflectOn[BothSidesWithOption].asInstanceOf[ScalaCaseClassInfo]
    assert(
      r.constructWith[BothSidesWithOption](List(Right(None))) == BothSidesWithOption(Right(None))
    )
    assert(
      r.constructWith[BothSidesWithOption](List(Right(Some("x")))) == BothSidesWithOption(Right(Some("x")))
    )
  }

  test("Scala Either with Union type") {
    val result = Reflector.reflectOn[BothSidesWithUnion]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.BothSidesWithUnion):
    |   fields:
    |      (0) a: Either:
    |         left--scala.Int
    |         right--Union:
    |            left--java.lang.String
    |            right--scala.Boolean""".stripMargin)
  }

  test("Scala Either with Union type assignment") {
    val r = Reflector.reflectOn[BothSidesWithUnion].asInstanceOf[ScalaCaseClassInfo]
    assert(
      r.constructWith[BothSidesWithUnion](List(Right("foo"))) == BothSidesWithUnion(Right("foo"))
    )
    assert(
      r.constructWith[BothSidesWithUnion](List(Right(true))) == BothSidesWithUnion(Right(true))
    )
  }

  test("Scala Either having a parameterized type") {
    val result = Reflector.reflectOn[BothSidesParam[Double]]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.BothSidesParam[Z]):
    |   fields:
    |      (0) a: Either:
    |         left--scala.Int
    |         right--Option of ScalaCaseClassInfo(co.blocke.dotty_reflection.ParamOption[T]):
    |            fields:
    |               (0) a: Option of scala.Double""".stripMargin)
  }