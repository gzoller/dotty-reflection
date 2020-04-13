package co.blocke.dotty_reflection

import munit._
import infos._
import PrimitiveType._
import java.util.Optional

class ParamTests extends munit.FunSuite {

  test("0-level param substitution") {
    val r = Reflector.reflectOn[DuoTypes[Int,Float]].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.DuoTypes",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",Scala_Int,_,_,None,Some("Q")), 
          ScalaFieldInfo(1,"b",Scala_Float,_,_,None,Some("U"))
        ),
        List("Q", "U"),
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }

  test("0-level Option substitution") {
    val r = Reflector.reflectOn[Option[WithDefault]].asInstanceOf[ScalaOptionInfo]
    val result = r match {
      case ScalaOptionInfo(
        "scala.Option",
        _,
        ScalaClassInfo(
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
        )
      ) => true
      case _ => false
    }
    assert(result)
  }

  test("0-level Either substitution") {
    val r = Reflector.reflectOn[Either[Int,WithDefault]].asInstanceOf[EitherInfo]
    val result = r match {
      case EitherInfo(
        "scala.util.Either",
        _,
        Scala_Int,
        ScalaClassInfo(
          "co.blocke.dotty_reflection.WithDefault",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"a",Scala_Int,_,_,None,None), 
            ScalaFieldInfo(1,"b",Scala_String,_,_,Some(_),None)
          ),
          Nil,
          _,false
        )
      ) => true
      case _ => false
    }
    assert(result)
  }

  test("0-level Map substitution") {
    val r = Reflector.reflectOn[Map[Int,WithDefault]].asInstanceOf[MapLikeInfo]
    val result = r match {
      case MapLikeInfo(
        "scala.collection.immutable.Map",
        _,
        Scala_Int,
        ScalaClassInfo(
          "co.blocke.dotty_reflection.WithDefault",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"a",Scala_Int,_,_,None,None), 
            ScalaFieldInfo(1,"b",Scala_String,_,_,Some(_),None)
          ),
          Nil,
          _,
          false)
       ) => true
      case _ => false
    }
    assert(result)
  }

  test("0-level List (Seq) substitution") {
    val r = Reflector.reflectOn[List[WithDefault]].asInstanceOf[SeqLikeInfo]
    val result = r match {
      case SeqLikeInfo(
        "scala.collection.immutable.List",
        _,
        ScalaClassInfo(
          "co.blocke.dotty_reflection.WithDefault",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"a",Scala_Int,_,_,None,None), 
            ScalaFieldInfo(1,"b",Scala_String,_,_,Some(_),None)
          ),
          Nil,
          _,
          false)
      ) => true
      case _ => false
    }
    assert(result)
  }

  test("0-level Try substitution") {
    val r = Reflector.reflectOn[scala.util.Try[WithDefault]].asInstanceOf[TryInfo]
    val result = r match {
      case TryInfo(
        "scala.util.Try",
        _,
        ScalaClassInfo(
          "co.blocke.dotty_reflection.WithDefault",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"a",Scala_Int,_,_,None,None), 
            ScalaFieldInfo(1,"b",Scala_String,_,_,Some(_),None)
          ),
          Nil,
          _,
          false)
      ) => true
      case _ => false
    }
    assert(result)  
  }

  test("0-level Trait substitution") {
    val r = Reflector.reflectOn[ParamThing[WithDefault]].asInstanceOf[TraitInfo]
    val result = r match {
      case TraitInfo(
        "co.blocke.dotty_reflection.ParamThing",
        _,
        List("X"),
        List(
          ScalaClassInfo(
            "co.blocke.dotty_reflection.WithDefault",
            _,
            Nil,
            List(
              ScalaFieldInfo(0,"a",Scala_Int,_,_,None,None), 
              ScalaFieldInfo(1,"b",Scala_String,_,_,Some(_),None)
            ),
            Nil,
            _,
            false)
        )
      ) => true
      case _ => false
    }
    assert(result)
  }
  
  test("0-level Tuple substitution") {
    val r = Reflector.reflectOn[(Int,Boolean)].asInstanceOf[TupleInfo]
    val result = r match {
      case TupleInfo("scala.Tuple2",_,List(Scala_Int, Scala_Boolean),List(Some("T1"),Some("T2"))) => true
      case _ => false
    }
    assert(result)
  }

  test("0-level Union substitution") {
    val r = Reflector.reflectOn[String | WithDefault].asInstanceOf[UnionInfo]
    val result = r match {
      case UnionInfo(
        Reflector.UNION_CLASS,
        Scala_String,
        ScalaClassInfo(
          "co.blocke.dotty_reflection.WithDefault",
          _,
          Nil,
          List(
            ScalaFieldInfo(0,"a",Scala_Int,_,_,None,None), 
            ScalaFieldInfo(1,"b",Scala_String,_,_,Some(_),None)
          ),
          Nil,
          _,
          false)
    ) => true
      case _ => false
    }
    assert(result)
  }

  test("0-level Intersection substitution") {    
    val r = Reflector.reflectOn[Stackable[Int] & Floatable[String]].asInstanceOf[IntersectionInfo]
    val result = r match {
      case IntersectionInfo(
        Reflector.INTERSECTION_CLASS,
        TraitInfo(
          "co.blocke.dotty_reflection.Stackable",
          _,
          List("T"),
          List(Scala_Int)
        ),
        TraitInfo(
          "co.blocke.dotty_reflection.Floatable",
          _,
          List("U"),
          List(Scala_String)
        )
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
        Nil,
        List(
          ScalaFieldInfo(0,"a",
            ScalaClassInfo("co.blocke.dotty_reflection.DuoTypes",
              _,
              Nil,
              List(
                ScalaFieldInfo(0,"a",Scala_Int,_,_,None,Some("Q")), 
                ScalaFieldInfo(1,"b",Scala_Float,_,_,None,Some("U"))
              ),
              List("Q","U"),
              _,
              false),
            _,_,None,None)
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
        Nil,
        List(
          ScalaFieldInfo(0,"a",
            ScalaOptionInfo(
              "scala.Option",
              _,
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                Nil,
                List(
                  ScalaFieldInfo(0,"a",Scala_String,_,_,None,Some("Q")), 
                  ScalaFieldInfo(1,"b",Scala_Boolean,_,_,None,Some("U"))
                ),
                List("Q","U"),
                _,
                false)
            ),
            _,_,None,None)
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
        Nil,
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
                  Nil,
                  List(
                    ScalaFieldInfo(0,"a",Scala_String,_,_,None,Some("Q")), 
                    ScalaFieldInfo(1,"b",Scala_Boolean,_,_,None,Some("U"))
                  ),
                  List("Q","U"),
                  _,
                  false)
              ),
            ),
             _,_,None,None)
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
        Nil,
        List(
          ScalaFieldInfo(0,"a",
            EitherInfo(
              "scala.util.Either",
              _,
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                Nil,
                List(
                  ScalaFieldInfo(0,"a",Scala_Int,_,_,None,Some("Q")), 
                  ScalaFieldInfo(1,"b",Scala_Float,_,_,None,Some("U"))
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
                  Nil,
                  List(
                    ScalaFieldInfo(0,"a",Scala_String,_,_,None,Some("Q")), 
                    ScalaFieldInfo(1,"b",Scala_Boolean,_,_,None,Some("U"))
                  ),
                  List("Q", "U"),
                  _,
                  false
                )
              )
            ),
            _,_,None,None)
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
        Nil,
        List(
          ScalaFieldInfo(0,"a",AliasInfo(
            "co.blocke.dotty_reflection.Model$package.mystery",
            ScalaClassInfo(
              "co.blocke.dotty_reflection.DuoTypes",
              _,
              Nil,
              List(
                ScalaFieldInfo(0,"a",Scala_Byte,_,_,None,Some("Q")), 
                ScalaFieldInfo(1,"b",Scala_Short,_,_,None,Some("U"))
              ),
              List("Q", "U"),
              _,
              false
            )
          ),
          _,_,None,None)
        ),
        Nil,
        _,
        false) => true
        case _ => false
    }
    assert(result)
  }
  
  test("2nd level subsitution in a class field") {
    val r = Reflector.reflectOn[DuoClass].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.DuoClass",
        _,
        Nil,
        List(
          ScalaFieldInfo(
            0,
            "a",
            ScalaClassInfo(
              "co.blocke.dotty_reflection.DuoTypes",
              _,
              Nil,
              List(
                ScalaFieldInfo(0,"a",Scala_Int,_,_,None,Some("Q")), 
                ScalaFieldInfo(
                  1,
                  "b",
                  ScalaClassInfo(
                    "co.blocke.dotty_reflection.DuoTypes",
                    _,
                    Nil,
                    List(
                      ScalaFieldInfo(0,"a",Scala_Byte,_,_,None,Some("Q")), 
                      ScalaFieldInfo(1,"b",Scala_Short,_,_,None,Some("U"))
                    ),
                    List("Q", "U"),
                    _,
                    false
                  ),
                  _,
                  _,
                  None,
                  Some("U")
                )
              ),
              List("Q", "U"),
              _,
              false
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

  test("List and Map subsitituion") {
    val r = Reflector.reflectOn[ListMapSub].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.ListMapSub",
        _,
        Nil,
        List(
          ScalaFieldInfo(0,"a",
            SeqLikeInfo(
              "scala.collection.immutable.List",
              _,
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                Nil,
                List(
                  ScalaFieldInfo(0,"a",Scala_Int,_,_,None,Some("Q")), 
                  ScalaFieldInfo(1,"b",Scala_Byte,_,_,None,Some("U"))
                ),
                List("Q", "U"),
                _,
                false
              ),
            ),
            _,_,None,None
          ), 
          ScalaFieldInfo(1,"b",
            MapLikeInfo(
              "scala.collection.immutable.Map",
              _,
              Scala_String,
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                Nil,
                List(
                  ScalaFieldInfo(0,"a",Scala_Float,_,_,None,Some("Q")), 
                  ScalaFieldInfo(1,"b",Scala_Short,_,_,None,Some("U"))
                ),
                List("Q", "U"),
                _,
                false
              ),
            ),
            _,_,None,None
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
        Nil,
        List(
          ScalaFieldInfo(0,"a",
            TryInfo(
              "scala.util.Try",_,
              ScalaClassInfo(
                "co.blocke.dotty_reflection.DuoTypes",
                _,
                Nil,
                List(
                  ScalaFieldInfo(0,"a",Scala_String,_,_,None,Some("Q")), 
                  ScalaFieldInfo(1,"b",Scala_Int,_,_,None,Some("U"))
                ),
                List("Q", "U"),
                _,
                false
              )
            ),
            _,_,None,None
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
        Nil,
        List(
          ScalaFieldInfo(0,"a",
            TraitInfo("co.blocke.dotty_reflection.TypeShell",_,List("X"),List(Scala_Int)
          ),
          _,_,None,None)
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
        Nil,
        List(
          ScalaFieldInfo(0,"a",UnionInfo(Reflector.UNION_CLASS,Scala_Int,TraitInfo("co.blocke.dotty_reflection.TypeShell",_,List("X"),List(Scala_String))),_,_,None,None)
        ),
        Nil,
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }

  test("Type member substitutions") {
    val r = Reflector.reflectOn[Envelope[FancyBody,Boolean]].asInstanceOf[ScalaClassInfo]
    val result = r match {
      case ScalaClassInfo(
        "co.blocke.dotty_reflection.Envelope",
        _,
        List(
          TypeMember(
            "Giraffe",
            "T",
            ScalaClassInfo(
              "co.blocke.dotty_reflection.FancyBody",
              _,
              Nil,
              List(
                ScalaFieldInfo(
                  0,
                  "message",
                  Scala_String,
                  _,
                  _,
                  None,
                  None
                )
              ),
              Nil,
              _,
              false
            )
          )
        ),
        List(
          ScalaFieldInfo(
            0,
            "id",
            Scala_String,
            _,
            _,
            None,
            None
          ), 
          ScalaFieldInfo(
            1,
            "body",
            ScalaClassInfo(
              "co.blocke.dotty_reflection.FancyBody",
              _,
              Nil,
              List(
                ScalaFieldInfo(
                  0,
                  "message",
                  Scala_String,
                  _,
                  _,
                  None,
                  None
                )
              ),
              Nil,
              _,
              false
            ),
            _,
            _,
            None,
            Some("T")
          )
        ),
        List("T", "U"),
        _,
        false) => true
      case _ => false
    }
    assert(result)
  }
}