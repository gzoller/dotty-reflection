package co.blocke.dotty_reflection

import munit._
import model._
import PrimitiveType._
import java.util.Optional

class ParamTests extends munit.FunSuite {

  // TODO: This is wrong.  Need to detect Int/Float and resolve the ConcreteType ScalaClassInfo!
  test("0-level param substitution".ignore) {
    val r = Reflector.reflectOn[DuoTypes[Int,Float]].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.DuoTypes",
        _,
        List(
          ScalaFieldInfo(0,"a","Q",_,_,None), 
          ScalaFieldInfo(1,"b","U",_,_,None)
        ),
        List("Q","U"),
        _,
        false
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("1st level param substitution") {
    val r = Reflector.reflectOn[DuoHolder].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.DuoHolder",
        _,
        List(
          ScalaFieldInfo(0,"a",
            ScalaClassInfo("co.blocke.dotty_reflection.DuoTypes",_,
              List(
                ScalaFieldInfo(0,"a",Scala_Int,_,_,None), 
                ScalaFieldInfo(1,"b",Scala_Float,_,_,None)
              ),
              List("Q","U"),
              _,
              false),
            _,_,None)
        ),
        Nil,
        _,
        false
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("2nd level param substitution - Option") {
    val r = Reflector.reflectOn[OptHolder].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.OptHolder",
        _,
        List(
          ScalaFieldInfo(0,"a",
            ScalaOptionInfo(
              "scala.Option",
              _,
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                List(
                  ScalaFieldInfo(0,"a",Scala_String,_,_,None), 
                  ScalaFieldInfo(1,"b",Scala_Boolean,_,_,None)
                ),
                List("Q","U"),
                _,
                false)
            ),
            _,_,None)
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }

  test("3rd level param substitution - Option") {
    val r = Reflector.reflectOn[OptHolder2].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.OptHolder2",
        _,
        List(
          ScalaFieldInfo(0,"a",
            ScalaOptionInfo(
              "scala.Option",
              _,
              ScalaOptionInfo(
                "scala.Option",
                _,
                ScalaClassInfo(
                  "co.blocke.dotty_reflection.DuoTypes",
                  _,
                  List(
                    ScalaFieldInfo(0,"a",Scala_String,_,_,None), 
                    ScalaFieldInfo(1,"b",Scala_Boolean,_,_,None)
                  ),
                  List("Q","U"),
                  _,
                  false)
              ),
            ),
             _,_,None)
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }

  test("2nd and 3rd level param substitution - Either") {
    val r = Reflector.reflectOn[EitherHolder].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.EitherHolder",
        _,
        List(
          ScalaFieldInfo(0,"a",
            ScalaEitherInfo(
              "scala.util.Either",
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                List(
                  ScalaFieldInfo(0,"a",Scala_Int,_,_,None), 
                  ScalaFieldInfo(1,"b",Scala_Float,_,_,None)
                ),
                List("Q", "U"),
                _,
                false
              ),
              ScalaOptionInfo(
                "scala.Option",
                _,
                ScalaClassInfo(
                  "co.blocke.dotty_reflection.DuoTypes",
                  _,
                  List(
                    ScalaFieldInfo(0,"a",Scala_String,_,_,None), 
                    ScalaFieldInfo(1,"b",Scala_Boolean,_,_,None)
                  ),
                  List("Q", "U"),
                  _,
                  false
                )
              )
            ),
            _,_,None)
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }

  test("Opaque type alias type substitution (rare)") {
    val r = Reflector.reflectOn[AliasTypeSub].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.AliasTypeSub",
        _,
        List(
          ScalaFieldInfo(0,"a",AliasInfo(
            "co.blocke.dotty_reflection.Model$package.mystery",
            ScalaClassInfo(
              "co.blocke.dotty_reflection.DuoTypes",
              _,
              List(
                ScalaFieldInfo(0,"a",Scala_Byte,_,_,None), 
                ScalaFieldInfo(1,"b",Scala_Short,_,_,None)
              ),
              List("Q", "U"),
              _,
              false
            )
          ),
          _,_,None)
        ),
        Nil,
        _,
        false) => true
        case _ => false
    }
    assert(result)
  }
  
  test("2nd level ubsitution in a class field") {
    val r = Reflector.reflectOn[DuoClass].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.DuoClass",
        _,
        List(
          ScalaFieldInfo(0,"a",
            ScalaClassInfo(
              "co.blocke.dotty_reflection.DuoTypes",
              _,
              List(
                ScalaFieldInfo(0,"a",Scala_Int,_,_,None), 
                ScalaFieldInfo(1,"b",
                ScalaClassInfo(
                  "co.blocke.dotty_reflection.DuoTypes",
                  _,
                  List(
                    ScalaFieldInfo(0,"a",Scala_Byte,_,_,None), 
                    ScalaFieldInfo(1,"b",Scala_Short,_,_,None)
                  ),
                  List("Q", "U"),
                  _,
                  false
                ),
                _,_,None)
              ),
              List("Q", "U"),
              _,
              false
            ),
            _,_,None)
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }

  test("List and Map subsitituion") {
    val r = Reflector.reflectOn[ListMapSub].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.ListMapSub",
        _,
        List(
          ScalaFieldInfo(0,"a",
            Collection_A1_Info(
              "scala.collection.immutable.List",
              _,
              List("A"),
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                List(
                  ScalaFieldInfo(0,"a",Scala_Int,_,_,None), 
                  ScalaFieldInfo(1,"b",Scala_Byte,_,_,None)
                ),
                List("Q", "U"),
                _,
                false
              ),
            ),
            _,_,None
          ), 
          ScalaFieldInfo(1,"b",
            Collection_A2_Info(
              "scala.collection.immutable.Map",
              _,
              List("K", "V"),
              Scala_String,
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                List(
                  ScalaFieldInfo(0,"a",Scala_Float,_,_,None), 
                  ScalaFieldInfo(1,"b",Scala_Short,_,_,None)
                ),
                List("Q", "U"),
                _,
                false
              ),
            ),
            _,_,None
          )
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }

  test("Try type substitution") {
    val r = Reflector.reflectOn[TryHolder].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.TryHolder",
        _,
        List(
          ScalaFieldInfo(0,"a",
            TryInfo(
              "scala.util.Try",_,List("T"),
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                List(
                  ScalaFieldInfo(0,"a",Scala_String,_,_,None), 
                  ScalaFieldInfo(1,"b",Scala_Int,_,_,None)
                ),
                List("Q", "U"),
                _,
                false
              )
            ),
            _,_,None
          )
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }

  test("Trait type substitution") {
    val r = Reflector.reflectOn[TypeShellHolder].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.TypeShellHolder",
        _,
        List(
          ScalaFieldInfo(0,"a",
            TraitInfo("co.blocke.dotty_reflection.TypeShell",_,List("X"),List(Scala_Int)
          ),
          _,_,None)
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }

  test("Union type substitution") {
    val r = Reflector.reflectOn[UnionHolder].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.UnionHolder",
        _,
        List(
          ScalaFieldInfo(0,"a",StaticUnionInfo("__union_type__",Nil,Scala_Int,TraitInfo("co.blocke.dotty_reflection.TypeShell",_,List("X"),List(Scala_String))),_,_,None)
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }
}