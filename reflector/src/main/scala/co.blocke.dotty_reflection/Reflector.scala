package co.blocke.dotty_reflection

import scala.jdk.CollectionConverters._
import scala.quoted._
import scala.tasty.Reflection

opaque type TypeSymbol = String 

object Reflector:

  inline def diveInto[T]: String = ${ diveIntoImpl[T]() }

  def diveIntoImpl[T]()(implicit qctx: QuoteContext, ttype: scala.quoted.Type[T]): Expr[String] = 
    import qctx.tasty.{_, given _}
    Expr( unwindType(qctx.tasty)(typeOf[T]) )

  def unwindType(reflect: Reflection)(aType: reflect.Type): String =
    import reflect.{_, given _}

    aType match {
      case tr: TypeRef =>
        println("ANNOTS for "+tr.classSymbol.get+" = "+tr.classSymbol.get.annots)
        "foom!"

      case t => "Not Applicable... "+t.classSymbol.get.fullName
    }

