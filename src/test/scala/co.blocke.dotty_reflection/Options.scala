package co.blocke.dotty_reflection

import munit._
import model._
import PrimitiveType._

class Options extends munit.FunSuite {

  test("Scala optional field") {
    val r = Reflector.reflectOn[NormalOption].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.NormalOption",
        List(
          ScalaFieldInfo(0,"a",ScalaOptionInfo("scala.Option",Scala_Int),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Java optional field") {
    val r = Reflector.reflectOn[co.blocke.reflect.JavaOption1].asInstanceOf[StaticJavaClassInfo]
    val result = r match {
      case StaticJavaClassInfo(
        "co.blocke.reflect.JavaOption1",
        List(
          JavaFieldInfo(0, "fld", JavaOptionInfo("java.util.Optional", Scala_Int),_,_,_)
        ),
        Nil,
        _) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala nested optional field") {
    val r = Reflector.reflectOn[NestedOption].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.NestedOption",
        List(
          ScalaFieldInfo(0,"a",ScalaOptionInfo("scala.Option",ScalaOptionInfo("scala.Option",Scala_Int)),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Java nested optional field") {
    val r = Reflector.reflectOn[co.blocke.reflect.JavaOption2].asInstanceOf[StaticJavaClassInfo]
    val result = r match {
      case StaticJavaClassInfo(
        "co.blocke.reflect.JavaOption2",
        List(
          JavaFieldInfo(0, "fld", JavaOptionInfo("java.util.Optional", JavaOptionInfo("java.util.Optional", Scala_Int)),_,_,_)
        ),
        Nil,
        _) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala optional parameterized field") {
    val r = Reflector.reflectOn[ParamOption[_]].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.ParamOption",
        List(
          ScalaFieldInfo(0,"a",ScalaOptionInfo("scala.Option","T"),_,_,None)
        ),
        List("T"),
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Java optional parameterized field") {
    val r = Reflector.reflectOn[co.blocke.reflect.JavaOption3[_]].asInstanceOf[StaticJavaClassInfo]
    val result = r match {
      case StaticJavaClassInfo(
        "co.blocke.reflect.JavaOption3",
        List(
          JavaFieldInfo(0, "fld", JavaOptionInfo("java.util.Optional", "T"),_,_,_)
        ),
        List("T"),
        _) => true
      case _ => false
    }
    assert(result)
  }
}