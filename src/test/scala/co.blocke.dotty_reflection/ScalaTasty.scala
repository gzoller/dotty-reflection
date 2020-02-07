package co.blocke.dotty_reflection

import org.junit.Assert.assertEquals
import org.junit.Test
import co.blocke.reflect._

class ScalaTasty {
  @Test
  def reflectBasicWithUnion() = {
    assertEquals(Reflector.reflectOn[Person].toString,"StaticClassInfo(co.blocke.dotty_reflection.Person,List(ScalaFieldInfo(0,name,Scala_String,Map(),public java.lang.String co.blocke.dotty_reflection.Person.name(),None), ScalaFieldInfo(1,age,Scala_Int,Map(),public int co.blocke.dotty_reflection.Person.age(),None), ScalaFieldInfo(2,other,StaticUnionInfo(__union_type__,List(),List(Scala_Int, Scala_Boolean)),Map(),public java.lang.Object co.blocke.dotty_reflection.Person.other(),None)),List(),Map(),false)")
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
    val d = Reflector.reflectOn[Definitely]
    assertEquals(d.toString, "StaticClassInfo(co.blocke.dotty_reflection.Definitely,List(ScalaFieldInfo(0,id,Scala_Int,Map(),public int co.blocke.dotty_reflection.Definitely.id(),None), ScalaFieldInfo(1,stuff,Scala_Char,Map(),public char co.blocke.dotty_reflection.Definitely.stuff(),None)),List(),Map(),false)")
  }
  
  @Test
  def mixins() = {
    val m = Reflector.reflectOn[WithMix]
    assertEquals(m.asInstanceOf[StaticClassInfo].hasMixin("co.blocke.dotty_reflection.SJCapture"),true)
  }

  @Test
  def withAnnotations() = {
    val m = Reflector.reflectOn[WithAnnotation]
    assertEquals(m.toString,"StaticClassInfo(co.blocke.dotty_reflection.WithAnnotation,List(ScalaFieldInfo(0,id,Scala_String,Map(co.blocke.reflect.FieldAnno -> Map(idx -> 5)),public java.lang.String co.blocke.dotty_reflection.WithAnnotation.id(),None)),List(),Map(co.blocke.reflect.ClassAnno -> Map(name -> Foom)),false)")
  }

  @Test
  def parameterizedClass() = {
    val wp = WithParam(1,true)
    val p = Reflector.reflectOnClass(wp.getClass)
    assertEquals(p.toString,"StaticClassInfo(co.blocke.dotty_reflection.WithParam,List(ScalaFieldInfo(0,one,T,Map(),public java.lang.Object co.blocke.dotty_reflection.WithParam.one(),None), ScalaFieldInfo(1,two,U,Map(),public java.lang.Object co.blocke.dotty_reflection.WithParam.two(),None)),List(T, U),Map(),false)")
  }

  @Test
  def opaqueTypeAlias() = {
    val e = Reflector.reflectOn[Employee]
    assertEquals(e.toString,"StaticClassInfo(co.blocke.dotty_reflection.Employee,List(ScalaFieldInfo(0,eId,StaticAliasInfo(co.blocke.dotty_reflection.Model$package.EMP_ID,Scala_Int),Map(),public int co.blocke.dotty_reflection.Employee.eId(),None), ScalaFieldInfo(1,age,Scala_Int,Map(),public int co.blocke.dotty_reflection.Employee.age(),None)),List(),Map(),false)")
  }

  /*
  @Test
  def valueClassSupport() = {
    def e = Reflector.reflectOn[Employee2]
    println(">>> "+e)
    assertEquals(1,1)
  }
  */
}
