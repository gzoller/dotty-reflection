package co.blocke.dotty_reflection

import munit._
import model._
import PrimitiveType._

class Collections extends munit.FunSuite {

  test("Scala List") {
    val r = Reflector.reflectOn[Coll1].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll1",
        _,
        List(
          ScalaFieldInfo(0,"a",Collection_A1_Info("scala.collection.immutable.List",_,List("A"),Scala_String),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala Set") {
    val r = Reflector.reflectOn[Coll2].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll2",
        _,
        List(
          ScalaFieldInfo(0,"a",Collection_A1_Info("scala.collection.immutable.HashSet",_,List("A"),Scala_String),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala Map 1") {
    val r = Reflector.reflectOn[Coll3].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll3",
        _,
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.immutable.Map",_,Scala_String,Scala_Float),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala Map 2") {
    val r = Reflector.reflectOn[Coll4].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll4",
        _,
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.immutable.ListMap",_,Scala_String,Scala_Boolean),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala mutable List") {
    val r = Reflector.reflectOn[Coll1m].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll1m",
        _,
        List(
          ScalaFieldInfo(0,"a",Collection_A1_Info("scala.collection.mutable.ListBuffer",_,List("A"),Scala_String),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala mutable Set") {
    val r = Reflector.reflectOn[Coll2m].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll2m",
        _,
        List(
          ScalaFieldInfo(0,"a",Collection_A1_Info("scala.collection.mutable.HashSet",_,List("A"),Scala_String),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala mutable Map 1") {
    val r = Reflector.reflectOn[Coll3m].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll3m",
        _,
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.mutable.Map",_,Scala_String,Scala_Float),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Scala mutable Map 2") {
    val r = Reflector.reflectOn[Coll4m].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll4m",
        _,
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.mutable.ListMap",_,Scala_String,Scala_Boolean),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Nested Collections") {
    val r = Reflector.reflectOn[NestedColl].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.NestedColl",
        _,
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.immutable.Map",_,Scala_String,Collection_A1_Info("scala.collection.immutable.List",_,List("A"),ScalaOptionInfo("scala.Option",_,Scala_Int))),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }

  test("Tuples") {
    val r = Reflector.reflectOn[TupleTurtle[Boolean]].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.TupleTurtle",
        _,
        List(
          ScalaFieldInfo(
            0,
            "t",
            TupleInfo(
              "scala.Tuple4",
              _,
              List(
                Scala_Int, 
                Scala_Boolean, 
                Collection_A1_Info(
                  "scala.collection.immutable.List",
                  _,
                  List("A"),
                  Scala_String
                ), 
                ScalaClassInfo(
                  "co.blocke.dotty_reflection.NormalOption",
                  _,
                  List(
                    ScalaFieldInfo(0,"a",ScalaOptionInfo("scala.Option",_,Scala_Int),_,_,None)
                  ),
                  Nil,
                  _,
                  false
                )
              )
            ),
            _,
            _,
            None
          )
        ),
        List("Z"),
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }
}
