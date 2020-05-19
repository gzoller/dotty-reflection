package co.blocke.dotty_reflection

import scala.reflect.ClassTag
import scala.quoted._


case class TypeStructure( className: String, params: List[TypeStructure] )


/** Special case to get parameter structure (recursively) of top-level call to Reflection.reflectOn.  
 *  Because the top-level call happens outside of TastyInspection (it hasn't started at that point) if
 *  the target class is itself parameterized, the resolved types of those parameters are lost to erasure.
 *  The remedy is the analyzeType macro that extracts all the recursive information, which then can then
 *  then be unwound outside the macro and inspected.
 *
 *  One other ism.... so why not just inspect from inside the macro?  Due to classpath isms in macro 
 *  processing, you can't easily instantiate a project (i.e. non-Java library) class inside a macro.
 *  The solution is to build a hierchy of classname strings that can be unpacked and instantiated 
 *  outside the macro.  No awards for elegance, but it works.
 */
inline def analyzeType[T]: TypeStructure = ${ analyzeTypeImpl[T]() }


def analyzeTypeImpl[T]()(implicit qctx: QuoteContext, ttype:scala.quoted.Type[T]): Expr[TypeStructure] = 
  import qctx.tasty.{_, given _}

  given TypeStructuresLiftable as Liftable[TypeStructure] = new Liftable[TypeStructure] {
    /** Lift a `TypeStructure` into `'{ TypeStructure }` */
    def toExpr(structure: TypeStructure): QuoteContext ?=> Expr[TypeStructure] =
      '{ new TypeStructure(${Expr(structure.className)}, ${Expr(structure.params)}) }
  }

  def diveDeep(aType: Type): TypeStructure =
    aType match {
      case AppliedType(t,tob) =>
        val className = t.asInstanceOf[TypeRef].classSymbol.get.fullName
        val res = tob.map(_.asInstanceOf[TypeRef].classSymbol.get.fullName)
        val params = tob.map( tb => diveDeep(tb.asInstanceOf[Type]) )
        TypeStructure(className, params)
      case tr: TypeRef => 
        val className = tr.classSymbol.get.fullName
        if className == ENUM_CLASSNAME then
          TypeStructure(tr.qualifier.asInstanceOf[TypeRef].termSymbol.moduleClass.fullName.dropRight(1), Nil)
        else
          TypeStructure(className, Nil)
      case OrType(left,right) =>
        val resolvedLeft = diveDeep(left.asInstanceOf[Type])
        val resolvedRight = diveDeep(right.asInstanceOf[Type])
        TypeStructure(UNION_CLASS, List(resolvedLeft, resolvedRight))
      case AndType(left,right) =>
        val resolvedLeft = diveDeep(left.asInstanceOf[Type])
        val resolvedRight = diveDeep(right.asInstanceOf[Type])
        TypeStructure(INTERSECTION_CLASS, List(resolvedLeft, resolvedRight))
    }

  val dive = diveDeep(typeOf[T])
  Expr(dive)
