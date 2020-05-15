package co.blocke.dotty_reflection

import munit._
import co.blocke.reflect.{ClassAnno,FieldAnno}
import info._
import PrimitiveType._

class ScalaTasty extends munit.FunSuite:

  test("reflect basic Tasty class with union") {
    val result = RType.of[Person] 
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.Person):
    |   fields:
    |      (0) name: java.lang.String
    |      (1) age: scala.Int
    |      (2) other: Union:
    |         left--scala.Int
    |         right--scala.Boolean""".stripMargin)
  }

  test("create basic Tasty class") {
    val p = Reflector.reflectOn[Person]
    val person = p.asInstanceOf[ScalaCaseClassInfo].constructWith[Person](List("Frank", 35, 5))
    assertEquals(person, Person("Frank",35,5))
  }

  test("handle match types") {
    val result = Reflector.reflectOn[Definitely] 
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.Definitely):
    |   fields:
    |      (0) id: scala.Int
    |      (1) stuff: scala.Char""".stripMargin)
  }
  
  test("process mixins") {
    val m = Reflector.reflectOn[WithMix]
    assertEquals(m.asInstanceOf[ScalaCaseClassInfo].hasMixin("co.blocke.dotty_reflection.SJCapture"),true)
  }

  test("capture field and class annotations") {
    val result = Reflector.reflectOn[WithAnnotation] 
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.WithAnnotation):
    |   fields:
    |      (0) id: java.lang.String
    |         annotations: Map(co.blocke.reflect.FieldAnno -> Map(idx -> 5))
    |   annotations: Map(co.blocke.reflect.ClassAnno -> Map(name -> Foom))""".stripMargin)
  }

  test("handle parameterized class") {
    val wp = WithParam(1,true)
    val result = Reflector.reflectOnClass(wp.getClass) 
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.WithParam[T,U]):
    |   fields:
    |      (0)[T] one: T
    |      (1)[U] two: U""".stripMargin)
  }

  test("handle opaque type alias") {
    val result = Reflector.reflectOn[Employee]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.Employee):
    |   fields:
    |      (0) eId: alias EMP_ID defined as scala.Int
    |      (1) age: scala.Int""".stripMargin)
  }

  test("opaque type alias is a union type") {
    val result = Reflector.reflectOn[OpaqueUnion] 
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.OpaqueUnion):
    |   fields:
    |      (0) id: alias GEN_ID defined as Union:
    |         left--scala.Int
    |         right--java.lang.String""".stripMargin)
  }

  test("support value classes") {
    val result = Reflector.reflectOn[Employee2]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.Employee2):
    |   fields:
    |      (0) eId: ScalaCaseClassInfo--Value Class--(co.blocke.dotty_reflection.IdUser):
    |         fields:
    |            (0) id: scala.Int
    |      (1) age: scala.Int""".stripMargin)
  }

  test("detect default values in case class constructor fields") {
    val result = Reflector.reflectOn[WithDefault]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.WithDefault):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String""".stripMargin)
    val wd = result.asInstanceOf[ScalaCaseClassInfo]
    val newWd = wd.constructWith[WithDefault](List(5,wd.fields(1).defaultValueAccessor.get()))
    assertEquals(newWd, WithDefault(5))
  }

  test("plain class support") {
    val result = Reflector.reflectOn[PlainGood]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.PlainGood):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String
    |   non-constructor fields:""".stripMargin)
    interceptMessage[java.lang.Exception]("Class [co.blocke.dotty_reflection.PlainBad]: Non-case class constructor arguments must all be 'val'"){
      Reflector.reflectOn[PlainBad]
    }
  }

  test("all Scala primitive types") {
    val result = Reflector.reflectOn[ScalaPrimitives]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.ScalaPrimitives):
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
    |      (9) j: scala.Any""".stripMargin)
  }

  test("unknown class") {
    val result = Reflector.reflectOn[scala.math.BigDecimal]
    assertEquals( result.show(), """UnknownInfo(scala.math.BigDecimal)""")
  }

  test("Try type") {
    val result = Reflector.reflectOn[TryMe]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.TryMe):
    |   fields:
    |      (0) maybe: Try of scala.Boolean""".stripMargin)
  }

  test("sealed trait with case classes") {
    val result = Reflector.reflectOn[VehicleHolder]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.VehicleHolder):
    |   fields:
    |      (0) v: SealedTraitInfo(co.blocke.dotty_reflection.Vehicle):
    |         children:
    |            ScalaCaseClassInfo(co.blocke.dotty_reflection.Truck):
    |               fields:
    |                  (0) numberOfWheels: scala.Int
    |            ScalaCaseClassInfo(co.blocke.dotty_reflection.Car):
    |               fields:
    |                  (0) numberOfWheels: scala.Int
    |                  (1) color: java.lang.String
    |            ScalaCaseClassInfo(co.blocke.dotty_reflection.Plane):
    |               fields:
    |                  (0) numberOfEngines: scala.Int""".stripMargin)
  }

  test("sealed trait with case objects") {
    val result = Reflector.reflectOn[FlavorHolder]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.FlavorHolder):
    |   fields:
    |      (0) f: SealedTraitInfo(co.blocke.dotty_reflection.Flavor):
    |         children:
    |            ObjectInfo(co.blocke.dotty_reflection.Vanilla)
    |            ObjectInfo(co.blocke.dotty_reflection.Chocolate)
    |            ObjectInfo(co.blocke.dotty_reflection.Bourbon)""".stripMargin)
  }

  test("handle intersection types") {
    val result = Reflector.reflectOn[IntersectionHolder]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.IntersectionHolder):
    |   fields:
    |      (0) a: Intersection:
    |         left--Intersection:
    |            left--TraitInfo(co.blocke.dotty_reflection.InterA)
    |            right--TraitInfo(co.blocke.dotty_reflection.InterB)
    |         right--TraitInfo(co.blocke.dotty_reflection.InterC)""".stripMargin)
  }

  test("handle Scala non-case classes") {
    val result = Reflector.reflectOn[FoomNC]
    val target = result.show()
    assertEquals( result.show(0,false,true), """ScalaClassInfo(co.blocke.dotty_reflection.FoomNC):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String
    |   non-constructor fields:
    |      (_) age: scala.Int
    |         annotations: Map(co.blocke.reflect.FieldAnno -> Map(idx -> 2))
    |      (_) blah: scala.Boolean
    |         annotations: Map(co.blocke.reflect.FieldAnno -> Map(idx -> 5))
    |      (_) hey: scala.Int
    |         annotations: Map(co.blocke.reflect.Ignore -> Map())""".stripMargin)
  }

  test("Inheritance and Annotations") {
    val result = Reflector.reflectOn[InheritSimpleChild]
    val target = result.show()
    assertEquals( result.show(0,false,true), """ScalaClassInfo(co.blocke.dotty_reflection.InheritSimpleChild):
    |   fields:
    |      (0) extra: java.lang.String
    |      (1) one: java.lang.String
    |         annotations: Map(co.blocke.reflect.Change -> Map(name -> uno), co.blocke.reflect.DBKey -> Map())
    |   non-constructor fields:
    |      (_) bogus: java.lang.String
    |         annotations: Map(co.blocke.reflect.Ignore -> Map())
    |      (_) dontForget: scala.Int
    |      (_) dontseeme: scala.Int
    |         annotations: Map(co.blocke.reflect.Ignore -> Map())
    |      (_) foo: scala.Int
    |         annotations: Map(co.blocke.reflect.DBKey -> Map(index -> 99))
    |      (_) four: scala.Double
    |         annotations: Map(co.blocke.reflect.DBKey -> Map(index -> 2), co.blocke.reflect.Change -> Map(name -> quatro))
    |      (_) nada: scala.Double
    |         annotations: Map(co.blocke.reflect.Ignore -> Map())
    |      (_) three: scala.Boolean
    |      (_) two: scala.Int
    |         annotations: Map(co.blocke.reflect.Change -> Map(name -> foobar), co.blocke.reflect.DBKey -> Map(index -> 1))
    |      (_) unused: scala.Double
    |         annotations: Map(co.blocke.reflect.Ignore -> Map())""".stripMargin)
  }

  test("Inheritance and Parameterized Classes") {
    val result = Reflector.reflectOn[ParamChild[Boolean]]
    assertEquals( result.show(0,false,true), """ScalaClassInfo(co.blocke.dotty_reflection.ParamChild[T]):
    |   fields:
    |      (0)[T] thing: scala.Boolean
    |   non-constructor fields:
    |      (_)[T] cosa: scala.Boolean
    |      (_)[T] item: scala.Boolean""".stripMargin)
  }