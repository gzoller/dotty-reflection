package co.blocke.dotty_reflection

import munit._
import info._
import PrimitiveType._

class Collections extends munit.FunSuite:

  test("Scala List") {
    val result = Reflector.reflectOn[Coll1]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Coll1):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.immutable.List)[A]: java.lang.String
    |   value class: false""".stripMargin)
  }

  test("Scala Set") {
    val result = Reflector.reflectOn[Coll2]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Coll2):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.immutable.HashSet)[A]: java.lang.String
    |   value class: false""".stripMargin)
  }

  test("Scala Map 1") {
    val result = Reflector.reflectOn[Coll3]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Coll3):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.immutable.Map)[K,V]:
    |         java.lang.String
    |         scala.Float
    |   value class: false""".stripMargin)  
  }

  test("Scala Map 2") {
    val result = Reflector.reflectOn[Coll4]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Coll4):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.immutable.ListMap)[K,V]:
    |         java.lang.String
    |         scala.Boolean
    |   value class: false""".stripMargin)  
  }

  test("Scala mutable List") {
    val result = Reflector.reflectOn[Coll1m]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Coll1m):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.mutable.ListBuffer)[A]: java.lang.String
    |   value class: false""".stripMargin)
  }

  test("Scala mutable Set") {
    val result = Reflector.reflectOn[Coll2m]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Coll2m):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.mutable.HashSet)[A]: java.lang.String
    |   value class: false""".stripMargin)
  }

  test("Scala mutable Map 1") {
    val result = Reflector.reflectOn[Coll3m]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Coll3m):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.mutable.Map)[K,V]:
    |         java.lang.String
    |         scala.Float
    |   value class: false""".stripMargin)
  }

  test("Scala mutable Map 2") {
    val result = Reflector.reflectOn[Coll4m]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Coll4m):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.mutable.ListMap)[K,V]:
    |         java.lang.String
    |         scala.Boolean
    |   value class: false""".stripMargin)
  }

  test("Nested Collections") {
    val result = Reflector.reflectOn[NestedColl]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.NestedColl):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.immutable.Map)[K,V]:
    |         java.lang.String
    |         SeqLikeInfo(scala.collection.immutable.List)[A]: Option of scala.Int
    |   value class: false""".stripMargin)
  }

  test("Tuples") {
    val result = Reflector.reflectOn[TupleTurtle[Boolean]]
    println("<< pending >>")
    /*
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.TupleTurtle)[Z]:
    |   fields:
    |      (0) t: (
    |         scala.Int
    |         [Z]java.lang.Object
    |         SeqLikeInfo(scala.collection.immutable.List)[A]: java.lang.String
    |         ScalaClassInfo(co.blocke.dotty_reflection.NormalOption):
    |            fields:
    |               (0) a: Option of scala.Int
    |            value class: false
    |         )
    |   value class: false""".stripMargin)
    */
  }

  test("Scala Arrays") {
    val result = Reflector.reflectOn[WithScalaArray]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.WithScalaArray):
    |   fields:
    |      (0) list: array of array of scala.Char
    |      (1) x1: array of scala.Boolean
    |      (2) x2: array of scala.Byte
    |      (3) x3: array of scala.Char
    |      (4) x4: array of scala.Double
    |      (5) x5: array of scala.Float
    |      (6) x6: array of scala.Int
    |      (7) x7: array of scala.Long
    |      (8) x8: array of scala.Short
    |      (9) x9: array of java.lang.String
    |   value class: false""".stripMargin)
  }
