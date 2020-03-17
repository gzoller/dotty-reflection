package co.blocke.dotty_reflection
package impl

import scala.reflect.ClassTag
import model._
import scala.quoted._


case class ParamStructure( className: String, params: List[ParamStructure] )


/** Special case to get parameter structure (recursively) of top-level call to Reflection.reflectOn.  
 *  Because the top-level call happens outside of TastyInspection (it hasn't started at that point) if
 *  the target class is itself parameterized, the resolved types of those parameters are lost to erasure.
 *  The remedy is the getParams macro that extracts all the recursive information, which then can then
 *  then be unwound outside the macro and inspected.
 *
 *  One other ism.... so why not just inspect from inside the macro?  Due to classpath isms in macro 
 *  processing, you can't easily instantiate a project (i.e. non-Java library) class inside a macro.
 *  The solution is to build a hierchy of classname strings that can be unpacked and instantiated 
 *  outside the macro.  No awards for elegance, but it works.
 */
inline def getParams[T]: ParamStructure = ${ getParamsImpl[T]() }


def getParamsImpl[T]()(implicit qctx: QuoteContext, ttype:scala.quoted.Type[T]): Expr[ParamStructure] = 
  import qctx.tasty.{_, given _}

  given ParamStructuresLiftable as Liftable[ParamStructure] = new Liftable[ParamStructure] {
    /** Lift a `ParamStructure` into `'{ ParamStructure }` */
    def toExpr(structure: ParamStructure): QuoteContext ?=> Expr[ParamStructure] =
      '{ new ParamStructure(${Expr(structure.className)}, ${Expr(structure.params)}) }
  }

  def diveDeep(aType: Type): ParamStructure =
    aType match {
      case AppliedType(t,tob) =>
        val className = t.asInstanceOf[TypeRef].classSymbol.get.fullName
        val res = tob.map(_.asInstanceOf[TypeRef].classSymbol.get.fullName)
        val params = tob.map( tb => diveDeep(tb.asInstanceOf[Type]) )
        ParamStructure(className, params)
      case tr: TypeRef => ParamStructure(tr.classSymbol.get.fullName, Nil)
    }

  val dive = diveDeep(typeOf[T])
  Expr(dive)
