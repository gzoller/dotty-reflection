package co.blocke.dotty_reflection

import munit._
import info._
import PrimitiveType._
import java.util.Optional

class Options extends munit.FunSuite:

  test("Scala optional field") {
    val result = Reflector.reflectOn[NormalOption]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.NormalOption):
    |   fields:
    |      (0) a: Option of scala.Int
    |   value class: false""".stripMargin)
  }

  test("Java optional field") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaOption1]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption1):
    |   fields:
    |      (0) fld: Optional of java.lang.Integer""".stripMargin)
  }

  test("Scala nested optional field") {
    val result = Reflector.reflectOn[NestedOption]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.NestedOption):
    |   fields:
    |      (0) a: Option of Option of scala.Int
    |   value class: false""".stripMargin)
  }

  test("Java nested optional field") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaOption2]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption2):
    |   fields:
    |      (0) fld: Optional of Optional of java.lang.Integer""".stripMargin)
  }

  test("Scala optional parameterized field") {
    val result = Reflector.reflectOn[ParamOption[Char]]
    println("<< pending >>")
    // val result = r match {
    //   case ScalaClassInfo(
    //     "co.blocke.dotty_reflection.ParamOption",
    //     _,
    //     Nil,
    //     List(
    //       ScalaFieldInfo(0,"a",ScalaOptionInfo("scala.Option",_,Scala_Char),_,_,None,None)
    //     ),
    //     List("T"),
    //     _,
    //     flase
    //     ) => true
    //   case _ => false
    // }
    // assert(result)
  }

  test("Java optional parameterized field") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaOption3[Char]]
    println("<< pending >>")
    // val result = r match {
    //   case JavaClassInfo(
    //     "co.blocke.reflect.JavaOption3",
    //     _,
    //     List(
    //       JavaFieldInfo(0, "fld", JavaOptionalInfo("java.util.Optional", _,Scala_Char),_,_,_,None)
    //     ),
    //     List("T"),
    //     _) => true
    //   case _ => false
    // }
    // assert(result)
  }

  test("Option assignments in union type") {
    val r = Reflector.reflectOn[UnionHavingOption].concreteType.asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[UnionHavingOption](List(None,Optional.empty())) == UnionHavingOption(None,Optional.empty())
    )
    assert(
      r.constructWith[UnionHavingOption](List(Some(3),Optional.of(3))) == UnionHavingOption(Some(3),Optional.of(3))
    )
  }

  test("Option of a union") {    
    val result = Reflector.reflectOn[OptionHavingUnion]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.OptionHavingUnion):
    |   fields:
    |      (0) a: Option of Union:
    |         left--scala.Boolean
    |         right--java.lang.String
    |   value class: false""".stripMargin)
  }

  test("Option of a union assignment") {    
    val r = Reflector.reflectOn[OptionHavingUnion].concreteType.asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[OptionHavingUnion](List(None)) == OptionHavingUnion(None)
    )
    assert(
      r.constructWith[OptionHavingUnion](List(Some(true))) == OptionHavingUnion(Some(true))
    )
    assert(
      r.constructWith[OptionHavingUnion](List(Some("wow"))) == OptionHavingUnion(Some("wow"))
    )
  }