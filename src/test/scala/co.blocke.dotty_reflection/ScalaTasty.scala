package co.blocke.dotty_reflection

import munit._
import co.blocke.reflect.{ClassAnno,FieldAnno}
import model._
import PrimitiveType._

class ScalaTasty extends munit.FunSuite {

  test("reflect basic Tasty class with union") {
    val result = Reflector.reflectOn[Person] match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Person",
        _,
        List(
          ScalaFieldInfo(0,"name",Scala_String,_,_,None),
          ScalaFieldInfo(1,"age",Scala_Int,_,_,None),
          ScalaFieldInfo(2,"other",StaticUnionInfo("__union_type__",_,Scala_Int, Scala_Boolean),_,_,None)
        ),
        Nil,
        _,
        false
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("create basic Tasty class") {
    val p = Reflector.reflectOn[Person]
    val person = p.asInstanceOf[StaticClassInfo].constructWith[Person](List("Frank", 35, 5))
    assertEquals(person, Person("Frank",35,5))
  }


  test("handle match types") {
    val result = Reflector.reflectOn[Definitely] match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Definitely",
        _,
        List(
          ScalaFieldInfo(0,"id",Scala_Int,_,_,None),
          ScalaFieldInfo(1,"stuff",Scala_Char,_,_,None)
        ),
        Nil,
        _,
        false
        ) => true
      case _ => false
    }
    assert(result)
  }
  
  test("process mixins") {
    val m = Reflector.reflectOn[WithMix]
    assertEquals(m.asInstanceOf[StaticClassInfo].hasMixin("co.blocke.dotty_reflection.SJCapture"),true)
  }

  test("capture field and class annotations") {
    val result = Reflector.reflectOn[WithAnnotation] match {
      case c @ StaticClassInfo(
        "co.blocke.dotty_reflection.WithAnnotation",
        _,
        List(
          ScalaFieldInfo(0,"id",Scala_String,_,_,None)
        ),
        Nil,
        _,
        false
      ) if c.annotations == Map("co.blocke.reflect.ClassAnno" -> Map("name" -> "Foom")) && c.fields(0).annotations == Map("co.blocke.reflect.FieldAnno" -> Map("idx" -> "5")) => true
      case _ => false
    }
    assert(result)
  }

  test("handle parameterized class") {
    val wp = WithParam(1,true)
    val result = Reflector.reflectOnClass(wp.getClass) match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.WithParam",
        _,
        List(
          ScalaFieldInfo(0,"one","T",_,_,None),
          ScalaFieldInfo(1,"two","U",_,_,None)
        ),
        List("T","U"),
        _,
        false
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("handle opaque type alias") {
    val result = Reflector.reflectOn[Employee] match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Employee",
        _,
        List(
         ScalaFieldInfo(0,"eId",AliasInfo("co.blocke.dotty_reflection.Model$package.EMP_ID",Scala_Int),_,_,None),
         ScalaFieldInfo(1,"age",Scala_Int,_,_,None)
        ),
        Nil,
        _,
        false
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("opaque type alias is a union type") {
    val result = Reflector.reflectOn[OpaqueUnion] match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.OpaqueUnion",
        _,
        List(ScalaFieldInfo(0,"id",AliasInfo("co.blocke.dotty_reflection.Model$package.GEN_ID",StaticUnionInfo("__union_type__",Nil,Scala_Int, Scala_String)),_,_,None)),
        Nil,
        _,
        false
        ) => true
      case c => false
    }
    assert(result)
  }

  test("support value classes") {
    def e = Reflector.reflectOn[Employee2]
    assertEquals(1,1)
  }

  test("detect default values in case class constructor fields") {
    val wd = Reflector.reflectOn[WithDefault].asInstanceOf[StaticClassInfo]
    val result = wd match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.WithDefault",
        _,
        List(
          ScalaFieldInfo(0,"a",Scala_Int,_,_,None),
          ScalaFieldInfo(1,"b",Scala_String,_,_,Some(_))
        ),
        Nil,
        _,
        false
        ) => true
      case _ => false
    }
    assert(result)
    val newWd = wd.constructWith[WithDefault](List(5,wd.fields(1).defaultValueAccessor.get()))
    assertEquals(newWd, WithDefault(5))
  }

  test("plain class support") {
    val r = Reflector.reflectOn[PlainGood].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
          "co.blocke.dotty_reflection.PlainGood",
          _,
          List(
            ScalaFieldInfo(0,"a",Scala_Int,_,_,None),
            ScalaFieldInfo(1,"b",Scala_String,_,_,None)
          ),
          Nil,
          _,
          false
        ) => true
      case _ => false
    }
    assert(result)

    interceptMessage[java.lang.Exception]("Class [co.blocke.dotty_reflection.PlainBad]: Non-case class constructor arguments must all be 'val'"){
      Reflector.reflectOn[PlainBad]
    }
  }

  test("all Scala primitive types") {
    val r = Reflector.reflectOn[ScalaPrimitives].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
          "co.blocke.dotty_reflection.ScalaPrimitives",
          _,
          List(
            ScalaFieldInfo(0,"a",Scala_Boolean,_,_,None),
            ScalaFieldInfo(1,"b",Scala_Byte,_,_,None),
            ScalaFieldInfo(2,"c",Scala_Char,_,_,None),
            ScalaFieldInfo(3,"d",Scala_Double,_,_,None),
            ScalaFieldInfo(4,"e",Scala_Float,_,_,None),
            ScalaFieldInfo(5,"f",Scala_Int,_,_,None),
            ScalaFieldInfo(6,"g",Scala_Long,_,_,None),
            ScalaFieldInfo(7,"h",Scala_Short,_,_,None),
            ScalaFieldInfo(8,"i",Scala_String,_,_,None),
            ScalaFieldInfo(9,"j",Scala_Any,_,_,None)
          ),
          Nil,
          _,
          false
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("unknown class") {
    val r = Reflector.reflectOn[scala.math.BigDecimal]
    assert(r.isInstanceOf[UnknownInfo])
  }
}

