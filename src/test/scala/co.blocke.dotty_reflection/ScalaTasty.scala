package co.blocke.dotty_reflection

import munit._
import co.blocke.reflect.{ClassAnno,FieldAnno}
import info._
import PrimitiveType._

class ScalaTasty extends munit.FunSuite:

  test("reflect basic Tasty class with union") {
    val result = Reflector.reflectOn[Person] 
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Person):
    |   fields:
    |      (0) name: java.lang.String
    |      (1) age: scala.Int
    |      (2) other: Union:
    |         left--scala.Int
    |         right--scala.Boolean
    |   value class: false""".stripMargin)
  }

  test("create basic Tasty class") {
    val p = Reflector.reflectOn[Person]
    val person = p.concreteType.asInstanceOf[ScalaClassInfo].constructWith[Person](List("Frank", 35, 5))
    assertEquals(person, Person("Frank",35,5))
  }

  test("handle match types") {
    val result = Reflector.reflectOn[Definitely] 
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Definitely):
    |   fields:
    |      (0) id: scala.Int
    |      (1) stuff: scala.Char
    |   value class: false""".stripMargin)
  }
  
  test("process mixins") {
    val m = Reflector.reflectOn[WithMix]
    assertEquals(m.concreteType.asInstanceOf[ScalaClassInfo].hasMixin("co.blocke.dotty_reflection.SJCapture"),true)
  }

  test("capture field and class annotations") {
    val result = Reflector.reflectOn[WithAnnotation] 
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.WithAnnotation):
    |   fields:
    |      (0) id: java.lang.String
    |         annotations: Map(co.blocke.reflect.FieldAnno -> Map(idx -> 5))   annotations: Map(co.blocke.reflect.ClassAnno -> Map(name -> Foom))
    |   value class: false""".stripMargin)
  }

  test("handle parameterized class") {
    val wp = WithParam(1,true)
    val result = Reflector.reflectOnClass(wp.getClass) 
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.WithParam)[T,U]:
    |   fields:
    |      (0) one: [T]java.lang.Object
    |      (1) two: [U]java.lang.Object
    |   value class: false""".stripMargin)
  }

  test("handle opaque type alias") {
    val result = Reflector.reflectOn[Employee]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Employee):
    |   fields:
    |      (0) eId: alias EMP_ID defined as scala.Int
    |      (1) age: scala.Int
    |   value class: false""".stripMargin)
  }

  test("opaque type alias is a union type") {
    val result = Reflector.reflectOn[OpaqueUnion] 
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.OpaqueUnion):
    |   fields:
    |      (0) id: alias GEN_ID defined as Union:
    |         left--scala.Int
    |         right--java.lang.String
    |   value class: false""".stripMargin)
  }

  test("support value classes") {
    val result = Reflector.reflectOn[Employee2]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.Employee2):
    |   fields:
    |      (0) eId: ScalaClassInfo(co.blocke.dotty_reflection.IdUser):
    |         fields:
    |            (0) id: scala.Int
    |         value class: true
    |      (1) age: scala.Int
    |   value class: false""".stripMargin)
  }

  test("detect default values in case class constructor fields") {
    val result = Reflector.reflectOn[WithDefault]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.WithDefault):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String
    |   value class: false""".stripMargin)
    val wd = result.concreteType.asInstanceOf[ScalaClassInfo]
    val newWd = wd.constructWith[WithDefault](List(5,wd.fields(1).defaultValueAccessor.get()))
    assertEquals(newWd, WithDefault(5))
  }

  test("plain class support") {
    val result = Reflector.reflectOn[PlainGood]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.PlainGood):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String
    |   value class: false""".stripMargin)
    interceptMessage[java.lang.Exception]("Class [co.blocke.dotty_reflection.PlainBad]: Non-case class constructor arguments must all be 'val'"){
      Reflector.reflectOn[PlainBad]
    }
  }

  test("all Scala primitive types") {
    val result = Reflector.reflectOn[ScalaPrimitives]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.ScalaPrimitives):
    |   fields:
    |      (0) a: scala.Boolean
    |      (1) b: scala.Byte
    |      (2) c: scala.Char
    |      (3) d: scala.Double
    |      (4) e: scala.Float
    |      (5) f: scala.Int
    |      (6) g: scala.Long
    |      (7) h: scala.Short
    |      (8) i: java.lang.String
    |      (9) j: scala.Any
    |   value class: false""".stripMargin)
  }

  test("unknown class") {
    val result = Reflector.reflectOn[scala.math.BigDecimal]
    assertEquals( result.show(), """UnknownInfo(scala.math.BigDecimal)""")
  }

  test("Try type") {
    val result = Reflector.reflectOn[TryMe]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.TryMe):
    |   fields:
    |      (0) maybe: Try of scala.Boolean
    |   value class: false""".stripMargin)
  }

  test("sealed trait with case classes") {
    val result = Reflector.reflectOn[VehicleHolder]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.VehicleHolder):
    |   fields:
    |      (0) v: SealedTraitInfo(co.blocke.dotty_reflection.Vehicle):
    |         children:
    |            ScalaClassInfo(co.blocke.dotty_reflection.Truck):
    |               fields:
    |                  (0) numberOfWheels: scala.Int
    |               value class: false
    |            ScalaClassInfo(co.blocke.dotty_reflection.Car):
    |               fields:
    |                  (0) numberOfWheels: scala.Int
    |                  (1) color: java.lang.String
    |               value class: false
    |            ScalaClassInfo(co.blocke.dotty_reflection.Plane):
    |               fields:
    |                  (0) numberOfEngines: scala.Int
    |               value class: false
    |   value class: false""".stripMargin)
  }

  test("sealed trait with case objects") {
    val result = Reflector.reflectOn[FlavorHolder]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.FlavorHolder):
    |   fields:
    |      (0) f: SealedTraitInfo(co.blocke.dotty_reflection.Flavor):
    |         children:
    |            ObjectInfo(co.blocke.dotty_reflection.Vanilla)
    |            ObjectInfo(co.blocke.dotty_reflection.Chocolate)
    |            ObjectInfo(co.blocke.dotty_reflection.Bourbon)
    |   value class: false""".stripMargin)
  }

  test("handle intersection types") {
    val result = Reflector.reflectOn[IntersectionHolder]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.IntersectionHolder):
    |   fields:
    |      (0) a: Intersection:
    |         left--Intersection:
    |            left--TraitInfo(co.blocke.dotty_reflection.InterA)
    |            right--TraitInfo(co.blocke.dotty_reflection.InterB)
    |         right--TraitInfo(co.blocke.dotty_reflection.InterC)
    |   value class: false""".stripMargin)
  }
