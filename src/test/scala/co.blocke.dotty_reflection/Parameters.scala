package co.blocke.dotty_reflection

import munit._
import info._
import PrimitiveType._
import java.util.Optional

class Parameters extends munit.FunSuite:

  test("0-level param substitution") {
    val result = Reflector.reflectOn[DuoTypes[Int,Float]].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |   fields:
    |      (0)[Q] a: scala.Int
    |      (1)[U] b: scala.Float""".stripMargin)
  }

  test("0-level Option substitution") {
    val result = Reflector.reflectOn[Option[WithDefault]].asInstanceOf[ScalaOptionInfo]
    assertEquals( result.show(), """Option of ScalaCaseClassInfo(co.blocke.dotty_reflection.WithDefault):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String""".stripMargin)
  }

  test("0-level Either substitution") {
    val result = Reflector.reflectOn[Either[Int,WithDefault]].asInstanceOf[EitherInfo]
    assertEquals( result.show(), """Either:
    |   left--scala.Int
    |   right--ScalaCaseClassInfo(co.blocke.dotty_reflection.WithDefault):
    |      fields:
    |         (0) a: scala.Int
    |         (1) b: java.lang.String""".stripMargin)
  }

  test("0-level Map substitution") {
    val result = Reflector.reflectOn[Map[Int,WithDefault]].asInstanceOf[MapLikeInfo]
    assertEquals( result.show(), """MapLikeInfo(scala.collection.immutable.Map[K,V]):
    |   scala.Int
    |   ScalaCaseClassInfo(co.blocke.dotty_reflection.WithDefault):
    |      fields:
    |         (0) a: scala.Int
    |         (1) b: java.lang.String""".stripMargin)
  }

  test("0-level List (Seq) substitution") {
    val result = Reflector.reflectOn[List[WithDefault]].asInstanceOf[SeqLikeInfo]
    assertEquals( result.show(), """SeqLikeInfo(scala.collection.immutable.List[A]): ScalaCaseClassInfo(co.blocke.dotty_reflection.WithDefault):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String""".stripMargin)
  }

  test("0-level Try substitution") {
    val result = Reflector.reflectOn[scala.util.Try[WithDefault]].asInstanceOf[TryInfo]
    assertEquals( result.show(), """Try of ScalaCaseClassInfo(co.blocke.dotty_reflection.WithDefault):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String""".stripMargin)
  }

  test("0-level Trait substitution") {
    val result = Reflector.reflectOn[ParamThing[WithDefault]].asInstanceOf[TraitInfo]
    assertEquals( result.show(), """TraitInfo(co.blocke.dotty_reflection.ParamThing[X]):
    |   actualParamTypes:
    |      [X] ScalaCaseClassInfo(co.blocke.dotty_reflection.WithDefault):
    |         fields:
    |            (0) a: scala.Int
    |            (1) b: java.lang.String""".stripMargin)
  }
  
  test("0-level Tuple substitution") {
    val result = Reflector.reflectOn[(Int,Boolean)].asInstanceOf[TupleInfo]
    assertEquals( result.show(), """(
    |   scala.Int
    |   scala.Boolean
    |)""".stripMargin)
  }

  test("0-level Union substitution") {
    val result = Reflector.reflectOn[String | WithDefault].asInstanceOf[UnionInfo]
    assertEquals( result.show(), """Union:
    |   left--java.lang.String
    |   right--ScalaCaseClassInfo(co.blocke.dotty_reflection.WithDefault):
    |      fields:
    |         (0) a: scala.Int
    |         (1) b: java.lang.String""".stripMargin)
  }

  test("0-level Intersection substitution") {    
    val result = Reflector.reflectOn[Stackable[Int] & Floatable[String]].asInstanceOf[IntersectionInfo]
    assertEquals( result.show(), """Intersection:
    |   left--TraitInfo(co.blocke.dotty_reflection.Stackable[T]):
    |      actualParamTypes:
    |         [T] scala.Int
    |   right--TraitInfo(co.blocke.dotty_reflection.Floatable[U]):
    |      actualParamTypes:
    |         [U] java.lang.String""".stripMargin)
  }

  test("1st level param substitution") {
    val result = Reflector.reflectOn[DuoHolder].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoHolder):
    |   fields:
    |      (0) a: ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |         fields:
    |            (0)[Q] a: scala.Int
    |            (1)[U] b: scala.Float""".stripMargin)
  }

  test("2nd level param substitution - Option") {
    val result = Reflector.reflectOn[OptHolder].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.OptHolder):
    |   fields:
    |      (0) a: Option of ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |         fields:
    |            (0)[Q] a: java.lang.String
    |            (1)[U] b: scala.Boolean""".stripMargin)
  }

  test("3rd level param substitution - Option") {
    val result = Reflector.reflectOn[OptHolder2].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.OptHolder2):
    |   fields:
    |      (0) a: Option of Option of ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |         fields:
    |            (0)[Q] a: java.lang.String
    |            (1)[U] b: scala.Boolean""".stripMargin)
  }

  test("2nd and 3rd level param substitution - Either") {
    val result = Reflector.reflectOn[EitherHolder].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.EitherHolder):
    |   fields:
    |      (0) a: Either:
    |         left--ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |            fields:
    |               (0)[Q] a: scala.Int
    |               (1)[U] b: scala.Float
    |         right--Option of ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |            fields:
    |               (0)[Q] a: java.lang.String
    |               (1)[U] b: scala.Boolean""".stripMargin)
  }

  test("Opaque type alias type substitution (rare)") {
    val result = Reflector.reflectOn[AliasTypeSub].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.AliasTypeSub):
    |   fields:
    |      (0) a: alias mystery defined as ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |         fields:
    |            (0)[Q] a: scala.Byte
    |            (1)[U] b: scala.Short""".stripMargin)
  }

  test("2nd level subsitution in a class field") {
    val result = Reflector.reflectOn[DuoClass].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoClass):
    |   fields:
    |      (0) a: ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |         fields:
    |            (0)[Q] a: scala.Int
    |            (1)[U] b: ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |               fields:
    |                  (0)[Q] a: scala.Byte
    |                  (1)[U] b: scala.Short""".stripMargin)
  }

  test("List and Map subsitituion") {
    val result = Reflector.reflectOn[ListMapSub].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.ListMapSub):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.immutable.List[A]): ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |         fields:
    |            (0)[Q] a: scala.Int
    |            (1)[U] b: scala.Byte
    |      (1) b: MapLikeInfo(scala.collection.immutable.Map[K,V]):
    |         java.lang.String
    |         ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |            fields:
    |               (0)[Q] a: scala.Float
    |               (1)[U] b: scala.Short""".stripMargin)
  }

  test("Try type substitution") {
    val result = Reflector.reflectOn[TryHolder]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.TryHolder):
    |   fields:
    |      (0) a: Try of ScalaCaseClassInfo(co.blocke.dotty_reflection.DuoTypes[Q,U]):
    |         fields:
    |            (0)[Q] a: java.lang.String
    |            (1)[U] b: scala.Int""".stripMargin)
  }

  test("Trait type substitution") {
    val result = Reflector.reflectOn[TypeShellHolder]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.TypeShellHolder):
    |   fields:
    |      (0) a: TraitInfo(co.blocke.dotty_reflection.TypeShell[X]):
    |         actualParamTypes:
    |            [X] scala.Int""".stripMargin)
  }

  test("Union type substitution") {
    val result = Reflector.reflectOn[UnionHolder]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.UnionHolder):
    |   fields:
    |      (0) a: Union:
    |         left--scala.Int
    |         right--TraitInfo(co.blocke.dotty_reflection.TypeShell[X]):
    |            actualParamTypes:
    |               [X] java.lang.String""".stripMargin)
  }

  test("Type member substitutions") {
    val result = Reflector.reflectOn[Envelope[FancyBody,Boolean]]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.Envelope[T,U]):
    |   fields:
    |      (0) id: java.lang.String
    |      (1)[T] body: ScalaCaseClassInfo(co.blocke.dotty_reflection.FancyBody):
    |         fields:
    |            (0) message: java.lang.String
    |   type members:
    |      Giraffe[T]: ScalaCaseClassInfo(co.blocke.dotty_reflection.FancyBody):
    |         fields:
    |            (0) message: java.lang.String""".stripMargin)
  }

  /* Performance tests
  var rtx: RType = null
  test("a") {
    rtx = Reflector.reflectOn[T10[T11[Int, T5[Double, Char]], String]]
  }
  test("a.1") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    Reflector.reflectOnClass(inst.getClass)
  }
  test("a.2") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    Reflector.reflectOnClassLite(inst.getClass)
  }
  test("a.3") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    Reflector.reflectOnClassLite(inst.getClass)
  }
  test("b") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    Reflector.reflectOnClassInTermsOf(inst.getClass, rtx)
  }
  test("c") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    Reflector.reflectOnClass(inst.getClass)
  }
  test("a2") {
    rtx = Reflector.reflectOn[T10[T11[Int, T5[Double, Char]], String]]
    println(rtx)
  }
  test("b2") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    println(Reflector.reflectOnClassInTermsOf(inst.getClass, rtx))
  }
  test("c2") { // roughly 0.6 sec, some of which may be saved by "lite" version of reflectOnClass
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    Reflector.reflectOnClass(inst.getClass)
  }
  */

  test("Nested trait substitutions") {
    val r = Reflector.reflectOn[T10[T11[Int, T5[Double, Char]], String]]
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    val result = Reflector.reflectOnClassInTermsOf( inst.getClass, r )
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.TFoo6[A,B,C,D]):
    |   fields:
    |      (0) x: TraitInfo(co.blocke.dotty_reflection.T11[W,Z]):
    |         actualParamTypes:
    |            [W] scala.Int
    |            [Z] TraitInfo(co.blocke.dotty_reflection.T5[X,Y]):
    |               actualParamTypes:
    |                  [X] scala.Double
    |                  [Y] scala.Char
    |      (1)[B] y: java.lang.String""".stripMargin)
  }