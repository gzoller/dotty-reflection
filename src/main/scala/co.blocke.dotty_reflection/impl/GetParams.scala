package co.blocke.dotty_reflection
package impl

import scala.reflect.ClassTag
import model._
import scala.quoted._

inline def getParams[T]: List[String] = ${ getParamsImpl[T]() }

def getParamsImpl[T]()(implicit qctx: QuoteContext, ttype:scala.quoted.Type[T]): Expr[List[String]] = 
  import qctx.tasty.{_, given _}

  def diveDeep(t: Type): ConcreteType =
    t match {
      case AppliedType(_,tob) =>
        val params = tob.map( tb => diveDeep(tb.asInstanceOf[Type]) )
        Reflector.reflectOnClassWithParams(clazz, params)
      case tr: TypeRef => Reflector.reflectOnClass(Class.forName(tr.classSymbol.get.fullName))
    }

  println("DIVE: "+diveDeep(typeOf[T]))

  /*
  typeOf[T] match {
    case AppliedType(_,tob) =>
      tob.map( t =>  )
  }

  val AppliedType(t,tob) = typeOf[T]

  val x = tob.head.asInstanceOf[TypeRef]
  println(x)
  // val cs = x.classSymbol.get
  val res = tob.map(_.asInstanceOf[TypeRef].classSymbol.get.fullName)
  Expr(res)
  */
  val res = List("foom")
  Expr(res)


  /*
  Foo[T]                --> TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scala),class Boolean)
  List[Foo[T]]          --> AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class dotty_reflection)),class Foo),List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scala),class Boolean)))
  List[Option[Foo[T]]]  --> AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scala),class Option),List(AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class dotty_reflection)),class Foo),List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scala),class Boolean)))))
  */
