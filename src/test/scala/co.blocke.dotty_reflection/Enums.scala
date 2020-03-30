package co.blocke.dotty_reflection

import munit._
import infos._
import PrimitiveType._

class Enums extends munit.FunSuite {

  test("Java Enums") {
    val r = Reflector.reflectOn[co.blocke.reflect.JavaEnum].asInstanceOf[JavaClassInfo]
    val result = r match {
      case JavaClassInfo(
          "co.blocke.reflect.JavaEnum",
          _,
          List(
            JavaFieldInfo(0,"color",JavaEnumInfo("co.blocke.reflect.Color",_),_,_,_)
          ),
          Nil,
          _
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala Enums (old and new)") {
    val r = Reflector.reflectOn[Birthday].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Birthday",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"m",ScalaEnum("co.blocke.dotty_reflection.Month",_),_,_,None), 
          ScalaFieldInfo(1,"d",ScalaEnumeration("co.blocke.dotty_reflection.WeekDay",_),_,_,None)
        ),
        Nil,
        _,
        false
        ) => true
      case _ => false
    }
    assert(result)
  }
}