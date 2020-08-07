package co.blocke.dotty_reflection

import munit._
import co.blocke.reflect.{ClassAnno,FieldAnno}
import info._
import impl.PrimitiveType._


class JavaNonTasty extends munit.FunSuite:

  test("basic Java collections") {
    val result = RType.of[JColl]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.JColl):
    |   fields:
    |      (0) a: JavaListInfo(java.util.List): scala.Int
    |      (1) b: Optional of JavaListInfo(java.util.ArrayList): scala.Int
    |      (2) c: JavaStackInfo(java.util.Stack): java.lang.String
    |      (3) d: JavaQueueInfo(java.util.Queue): MapLikeInfo(scala.collection.immutable.Map):
    |         scala.Int
    |         java.lang.String
    |      (4) e: JavaSetInfo(java.util.Set): scala.Boolean
    |      (5) f: JavaMapInfo(java.util.Map):
    |         scala.Int
    |         java.lang.String
    |""".stripMargin)
  }