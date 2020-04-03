package co.blocke.dotty_reflection

import munit._
import infos._
import PrimitiveType._

class Collections extends munit.FunSuite {

  test("Scala List") {
    val r = Reflector.reflectOn[Coll1]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll1",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",SeqLikeInfo("scala.collection.immutable.List",_,Scala_String),_,_,None)
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
    val r = Reflector.reflectOn[Coll2]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll2",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",SeqLikeInfo("scala.collection.immutable.HashSet",_,Scala_String),_,_,None)
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
    val r = Reflector.reflectOn[Coll3]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll3",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",MapLikeInfo("scala.collection.immutable.Map",_,Scala_String,Scala_Float),_,_,None)
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
    val r = Reflector.reflectOn[Coll4]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll4",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",MapLikeInfo("scala.collection.immutable.ListMap",_,Scala_String,Scala_Boolean),_,_,None)
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
    val r = Reflector.reflectOn[Coll1m]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll1m",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",SeqLikeInfo("scala.collection.mutable.ListBuffer",_,Scala_String),_,_,None)
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
    val r = Reflector.reflectOn[Coll2m]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll2m",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",SeqLikeInfo("scala.collection.mutable.HashSet",_,Scala_String),_,_,None)
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
    val r = Reflector.reflectOn[Coll3m]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll3m",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",MapLikeInfo("scala.collection.mutable.Map",_,Scala_String,Scala_Float),_,_,None)
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
    val r = Reflector.reflectOn[Coll4m]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Coll4m",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",MapLikeInfo("scala.collection.mutable.ListMap",_,Scala_String,Scala_Boolean),_,_,None)
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
    val r = Reflector.reflectOn[NestedColl]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.NestedColl",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",MapLikeInfo("scala.collection.immutable.Map",_,Scala_String,SeqLikeInfo("scala.collection.immutable.List",_,ScalaOptionInfo("scala.Option",_,Scala_Int))),_,_,None)
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
    val r = Reflector.reflectOn[TupleTurtle[Boolean]]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.TupleTurtle",
        _,
        Nil,
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
                SeqLikeInfo(
                  "scala.collection.immutable.List",
                  _,
                  Scala_String
                ), 
                ScalaClassInfo(
                  "co.blocke.dotty_reflection.NormalOption",
                  _,
                  Nil,
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

  test("Scala Arrays") {
    val r = Reflector.reflectOn[WithScalaArray]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.WithScalaArray",
        _,
        Nil,
        List(
          ScalaFieldInfo(
            0,
            "list",
            ArrayInfo(
              "[[C",
              _,
              ArrayInfo(
                "[C",
                _,
                Scala_Char
              )
            ),
            _,
            _,
            None
          ), 
          ScalaFieldInfo(
            1,
            "x1",
            ArrayInfo(
              "[Z",
              _,
              Scala_Boolean
            ),
            _,
            _,
            None
          ), 
          ScalaFieldInfo(
            2,
            "x2",
            ArrayInfo(
              "[B",
              _,
              Scala_Byte
            ),
            _,
            _,
            None
          ), 
          ScalaFieldInfo(
            3,
            "x3",
            ArrayInfo(
              "[C",
              _,
              Scala_Char
            ),
            _,
            _,
            None
          ), 
          ScalaFieldInfo(
            4,
            "x4",
            ArrayInfo(
              "[D",
              _,
              Scala_Double
            ),
            _,
            _,
            None
          ), 
          ScalaFieldInfo(
            5,
            "x5",
            ArrayInfo(
              "[F",
              _,
              Scala_Float
            ),
            _,
            _,
            None
          ), 
          ScalaFieldInfo(
            6,
            "x6",
            ArrayInfo(
              "[I",
              _,
              Scala_Int
            ),
            _,
            _,
            None
          ), 
          ScalaFieldInfo(
            7,
            "x7",
            ArrayInfo(
              "[J",
              _,
              Scala_Long
            ),
            _,
            _,
            None
          ), 
          ScalaFieldInfo(
            8,
            "x8",
            ArrayInfo(
              "[S",
              _,
              Scala_Short
            ),
            _,
            _,
            None
          ), 
          ScalaFieldInfo(
            9,
            "x9",
            ArrayInfo(
              "[Ljava.lang.String;",
              _,
              Scala_String
            ),
            _,
            _,
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
