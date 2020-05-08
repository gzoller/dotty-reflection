package co.blocke.dotty_reflection

import munit._
import info._
import PrimitiveType._

class Enums extends munit.FunSuite:

  test("Java Enums") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaEnum]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaEnum):
    |   fields:
    |      (0) color: JavaEnumInfo(co.blocke.reflect.Color)""".stripMargin)
  }

  test("Scala Enums (old and new)") {
    val result = Reflector.reflectOn[Birthday]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.Birthday):
    |   fields:
    |      (0) m: ScalaEnumInfo with values [Jan,Feb,Mar]
    |      (1) d: ScalaEnumerationInfo with values [Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday]""".stripMargin)
  }
