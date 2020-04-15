package co.blocke.dotty_reflection

import munit._
import info._
import PrimitiveType._
import scala.util.{Left,Right}

class Eithers extends munit.FunSuite:

  test("Scala simple Either field") {
    val result = Reflector.reflectOn[BothSides]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.BothSides):
    |   fields:
    |      (0) a: Either:
    |         left--scala.Int
    |         right--java.lang.String
    |   value class: false""".stripMargin)
  }

  test("Scala simple Either field assignment") {
    val r = Reflector.reflectOn[BothSides].concreteType.asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[BothSides](List(Right("Foom"))) == BothSides(Right("Foom"))
    )
    assert(
      r.constructWith[BothSides](List(Left(3))) == BothSides(Left(3))
    )
  }

  test("Scala Either with Option") {
    val result = Reflector.reflectOn[BothSidesWithOption]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.BothSidesWithOption):
    |   fields:
    |      (0) a: Either:
    |         left--scala.Int
    |         right--Option of java.lang.String
    |   value class: false""".stripMargin)
  }

  test("Scala Either with Option assignment") {
    val r = Reflector.reflectOn[BothSidesWithOption].concreteType.asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[BothSidesWithOption](List(Right(None))) == BothSidesWithOption(Right(None))
    )
    assert(
      r.constructWith[BothSidesWithOption](List(Right(Some("x")))) == BothSidesWithOption(Right(Some("x")))
    )
  }

  test("Scala Either with Union type") {
    val result = Reflector.reflectOn[BothSidesWithUnion]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.BothSidesWithUnion):
    |   fields:
    |      (0) a: Either:
    |         left--scala.Int
    |         right--Union:
    |            left--java.lang.String
    |            right--scala.Boolean
    |   value class: false""".stripMargin)
  }

  test("Scala Either with Union type assignment") {
    val r = Reflector.reflectOn[BothSidesWithUnion].concreteType.asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[BothSidesWithUnion](List(Right("foo"))) == BothSidesWithUnion(Right("foo"))
    )
    assert(
      r.constructWith[BothSidesWithUnion](List(Right(true))) == BothSidesWithUnion(Right(true))
    )
  }
