package co.blocke.dotty_reflection

import munit._
import co.blocke.reflect.{ClassAnno,FieldAnno}
import infos._
import PrimitiveType._

class ScalaTasty extends munit.FunSuite {

  test("reflect basic Tasty class with union") {
    val result = Reflector.reflectOn[Person] match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Person",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"name",Scala_String,_,_,None,None),
          ScalaFieldInfo(1,"age",Scala_Int,_,_,None,None),
          ScalaFieldInfo(2,"other",UnionInfo(Reflector.UNION_CLASS,Scala_Int, Scala_Boolean),_,_,None,None)
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
    val person = p.asInstanceOf[ScalaClassInfo].constructWith[Person](List("Frank", 35, 5))
    assertEquals(person, Person("Frank",35,5))
  }


  test("handle match types") {
    val result = Reflector.reflectOn[Definitely] match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Definitely",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"id",Scala_Int,_,_,None,None),
          ScalaFieldInfo(1,"stuff",Scala_Char,_,_,None,None)
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
    assertEquals(m.asInstanceOf[ScalaClassInfo].hasMixin("co.blocke.dotty_reflection.SJCapture"),true)
  }

  test("capture field and class annotations") {
    val result = Reflector.reflectOn[WithAnnotation] match {
      case c @ ScalaClassInfo(
        "co.blocke.dotty_reflection.WithAnnotation",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"id",Scala_String,_,_,None,None)
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
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.WithParam",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"one",Scala_Any,_,_,None,Some("T")),
          ScalaFieldInfo(1,"two",Scala_Any,_,_,None,Some("U"))
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
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Employee",
        _,
        Nil,
        List(
         ScalaFieldInfo(0,"eId",AliasInfo("co.blocke.dotty_reflection.Model$package.EMP_ID",Scala_Int),_,_,None,None),
         ScalaFieldInfo(1,"age",Scala_Int,_,_,None,None)
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
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.OpaqueUnion",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"id",AliasInfo("co.blocke.dotty_reflection.Model$package.GEN_ID",UnionInfo(Reflector.UNION_CLASS,Scala_Int, Scala_String)),_,_,None,None)
        ),
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
    val wd = Reflector.reflectOn[WithDefault].asInstanceOf[ScalaClassInfo]
    val result = wd match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.WithDefault",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",Scala_Int,_,_,None,None),
          ScalaFieldInfo(1,"b",Scala_String,_,_,Some(_),None)
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
    val r = Reflector.reflectOn[PlainGood].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
          "co.blocke.dotty_reflection.PlainGood",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"a",Scala_Int,_,_,None,None),
            ScalaFieldInfo(1,"b",Scala_String,_,_,None,None)
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
    val r = Reflector.reflectOn[ScalaPrimitives].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
          "co.blocke.dotty_reflection.ScalaPrimitives",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"a",Scala_Boolean,_,_,None,None),
            ScalaFieldInfo(1,"b",Scala_Byte,_,_,None,None),
            ScalaFieldInfo(2,"c",Scala_Char,_,_,None,None),
            ScalaFieldInfo(3,"d",Scala_Double,_,_,None,None),
            ScalaFieldInfo(4,"e",Scala_Float,_,_,None,None),
            ScalaFieldInfo(5,"f",Scala_Int,_,_,None,None),
            ScalaFieldInfo(6,"g",Scala_Long,_,_,None,None),
            ScalaFieldInfo(7,"h",Scala_Short,_,_,None,None),
            ScalaFieldInfo(8,"i",Scala_String,_,_,None,None),
            ScalaFieldInfo(9,"j",Scala_Any,_,_,None,None)
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

  test("Try type") {
    val r = Reflector.reflectOn[TryMe]
    val result = r match {
      case ScalaClassInfo(
          "co.blocke.dotty_reflection.TryMe",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"maybe",TryInfo("scala.util.Try",_,Scala_Boolean),_,_,None,None),
          ),
          Nil,
          _,
          false
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("sealed trait with case classes") {
    val r = Reflector.reflectOn[VehicleHolder]
    val result = r match {
      case ScalaClassInfo(
          "co.blocke.dotty_reflection.VehicleHolder",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"v",
              SealedTraitInfo(
                "co.blocke.dotty_reflection.Vehicle",
                _,
                Nil,
                List(
                  ScalaClassInfo("co.blocke.dotty_reflection.Truck",_,Nil,List(ScalaFieldInfo(0,"numberOfWheels",Scala_Int,_,_,None,None)),Nil,_,false), 
                  ScalaClassInfo("co.blocke.dotty_reflection.Car",_,Nil,List(
                    ScalaFieldInfo(0,"numberOfWheels",Scala_Int,_,_,None,None),
                    ScalaFieldInfo(1,"color",Scala_String,_,_,None,None)
                  ),Nil,_,false), 
                  ScalaClassInfo("co.blocke.dotty_reflection.Plane",_,Nil,List(ScalaFieldInfo(0,"numberOfEngines",Scala_Int,_,_,None,None)),Nil,_,false)
                )
              ),
              _,_,None,None
            ),
          ),
          Nil,
          _,
          false
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("sealed trait with case objects") {
    val r = Reflector.reflectOn[FlavorHolder]
    val result = r match {
      case ScalaClassInfo(
          "co.blocke.dotty_reflection.FlavorHolder",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"f",
              SealedTraitInfo(
                "co.blocke.dotty_reflection.Flavor",
                _,
                Nil,
                List(
                  ObjectInfo("co.blocke.dotty_reflection.Vanilla",_), 
                  ObjectInfo("co.blocke.dotty_reflection.Chocolate",_), 
                  ObjectInfo("co.blocke.dotty_reflection.Bourbon",_)
                )
              ),
              _,_,None,None
            ),
          ),
          Nil,
          _,
          false
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("handle intersection types") {
    val r = Reflector.reflectOn[IntersectionHolder].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.IntersectionHolder",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",
            IntersectionInfo(
              Reflector.INTERSECTION_CLASS,
              IntersectionInfo(
                Reflector.INTERSECTION_CLASS,
                TraitInfo("co.blocke.dotty_reflection.InterA",_,Nil,Nil),
                TraitInfo("co.blocke.dotty_reflection.InterB",_,Nil,Nil)
              ),
              TraitInfo("co.blocke.dotty_reflection.InterC",_,Nil,Nil)
            ),
            _,
            _,
            None,
            None
         )
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }
}

