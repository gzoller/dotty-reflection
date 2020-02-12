package co.blocke.dotty_reflection

import munit._
// import org.junit.Assert.assertEquals
// import org.junit.Test
import co.blocke.reflect.{ClassAnno,FieldAnno}
import model._
import PrimitiveType._

class ScalaTasty extends munit.FunSuite {
  test("reflect basic with union") {
    assertEquals(1,2)
    // assertEquals(Reflector.reflectOn[Person].toString,"StaticClassInfo(co.blocke.dotty_reflection.Person,List(ScalaFieldInfo(0,name,Scala_String,Map(),public java.lang.String co.blocke.dotty_reflection.Person.name(),None), ScalaFieldInfo(1,age,Scala_Int,Map(),public int co.blocke.dotty_reflection.Person.age(),None), ScalaFieldInfo(2,other,StaticUnionInfo(__union_type__,List(),List(Scala_Int, Scala_Boolean)),Map(),public java.lang.Object co.blocke.dotty_reflection.Person.other(),None)),List(),Map(),false)")
  }

  /*
  @Test
  def reflectBasicWithUnion() = {
    val result = Reflector.reflectOn[Person] match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Person",
        List(
          ScalaFieldInfo(0,"name",Scala_String,_,_,None),
          ScalaFieldInfo(1,"age",Scala_Int,_,_,None),
          ScalaFieldInfo(2,"other",StaticUnionInfo("__union_type__",_,List(Scala_Int, Scala_Boolean)),_,_,None)
        ),
        Nil,
        _,
        false
        ) => true
      case _ => false
    }
    assert(result)
  }

  @Test
  def basicCreation() = {
    val p = Reflector.reflectOn[Person]
    val person = p.asInstanceOf[StaticClassInfo].constructWith[Person](List("Frank", 35, 5))
    assertEquals(person, Person("Frank",35,5))
  }

  @Test(expected = classOf[java.lang.IllegalArgumentException])
  def wrongUnionType() = {
    val p = Reflector.reflectOn[Person]
    val person = p.asInstanceOf[StaticClassInfo].constructWith[Person](List("Frank", 35, 12.34))
    assertEquals(person, Person("Frank",35,5))
  }

  @Test
  def matchTypes() = {
    val result = Reflector.reflectOn[Definitely] match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Definitely",
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
  
  @Test
  def mixins() = {
    val m = Reflector.reflectOn[WithMix]
    assertEquals(m.asInstanceOf[StaticClassInfo].hasMixin("co.blocke.dotty_reflection.SJCapture"),true)
  }

  @Test
  def withAnnotations() = {
    val result = Reflector.reflectOn[WithAnnotation] match {
      case c @ StaticClassInfo(
        "co.blocke.dotty_reflection.WithAnnotation",
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

  @Test
  def parameterizedClass() = {
    val wp = WithParam(1,true)
    val result = Reflector.reflectOnClass(wp.getClass) match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.WithParam",
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

  @Test
  def opaqueTypeAlias() = {
    val result = Reflector.reflectOn[Employee] match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Employee",
        List(
         ScalaFieldInfo(0,"eId",StaticAliasInfo("co.blocke.dotty_reflection.Model$package.EMP_ID",Scala_Int),_,_,None),
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

  @Test
  def opaqueTypeIsUnion() = {
    val result = Reflector.reflectOn[OpaqueUnion] match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.OpaqueUnion",
        List(ScalaFieldInfo(0,"id",StaticAliasInfo("co.blocke.dotty_reflection.Model$package.GEN_ID",StaticUnionInfo("__union_type__",Nil,List(Scala_Int, Scala_String))),_,_,None)),
        Nil,
        _,
        false
        ) => true
      case c => false
    }
    assert(result)
  }

  @Test(expected = classOf[java.lang.IllegalArgumentException])
  def unionProtections() = {
    val ou = Reflector.reflectOn[OpaqueUnion]
    val materialized = ou.asInstanceOf[StaticClassInfo].constructWith[OpaqueUnion](List(true))
  }

  // TODO: Test union protections for inheritance / interface-trait tree!

  @Test
  def valueClassSupport() = {
    def e = Reflector.reflectOn[Employee2]
    assertEquals(1,1)
  }

  @Test
  def withDefault() = {
    val wd = Reflector.reflectOn[WithDefault].asInstanceOf[StaticClassInfo]
    val result = wd match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.WithDefault",
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
  */
}
