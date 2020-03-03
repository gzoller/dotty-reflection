package co.blocke.dotty_reflection

import munit._
import model._
import PrimitiveType._

class Collections extends munit.FunSuite {

  test("Scala List") {
    val r = Reflector.reflectOn[Coll1].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Coll1",
        List(
          ScalaFieldInfo(0,"a",Collection_A1_Info("scala.collection.immutable.List",List("A"),Scala_String),_,_,None)
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
    val r = Reflector.reflectOn[Coll2].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Coll2",
        List(
          ScalaFieldInfo(0,"a",Collection_A1_Info("scala.collection.immutable.HashSet",List("A"),Scala_String),_,_,None)
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
    val r = Reflector.reflectOn[Coll3].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Coll3",
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.immutable.Map",List("K","V"),Scala_String,Scala_Float),_,_,None)
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
    val r = Reflector.reflectOn[Coll4].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Coll4",
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.immutable.ListMap",List("K","V"),Scala_String,Scala_Boolean),_,_,None)
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
    val r = Reflector.reflectOn[Coll1m].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Coll1m",
        List(
          ScalaFieldInfo(0,"a",Collection_A1_Info("scala.collection.mutable.ListBuffer",List("A"),Scala_String),_,_,None)
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
    val r = Reflector.reflectOn[Coll2m].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Coll2m",
        List(
          ScalaFieldInfo(0,"a",Collection_A1_Info("scala.collection.mutable.HashSet",List("A"),Scala_String),_,_,None)
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
    val r = Reflector.reflectOn[Coll3m].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Coll3m",
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.mutable.Map",List("K","V"),Scala_String,Scala_Float),_,_,None)
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
    val r = Reflector.reflectOn[Coll4m].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.Coll4m",
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.mutable.ListMap",List("K","V"),Scala_String,Scala_Boolean),_,_,None)
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
    val r = Reflector.reflectOn[NestedColl].asInstanceOf[StaticClassInfo]
    val result = r match {
      case StaticClassInfo(
        "co.blocke.dotty_reflection.NestedColl",
        List(
          ScalaFieldInfo(0,"a",Collection_A2_Info("scala.collection.immutable.Map",List("K","V"),Scala_String,Collection_A1_Info("scala.collection.immutable.List",List("A"),ScalaOptionInfo("scala.Option",Scala_Int))),_,_,None)
        ),
        Nil,
        _,
        flase
        ) => true
      case _ => false
    }
    assert(result)
  }
}
