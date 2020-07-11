package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tools.dotc.ast.Trees.AppliedTypeTree
  
class TastyInspection[T](clazz: Class[_], inTermsOf: Option[TraitInfo] = None) extends TastyInspector:

  var inspected: RType = UnknownInfo(clazz.getName)

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}
    inTermsOf match {
      case Some(ito) =>
        println("HEY: "+ito.actualParameterTypes.toList)
        // PROBLEM:  Actual types come across the compile->runtime bridge as RTypes in actualParameterTypes.
        //  But... unless the type is simple (e.g. primitive) valuable parameters get lost during Type(p.infoClass).
        //  In other words Level1[Int,Boolean] merely becomes Type(Level1.class) (no params)
        //  Hmm...
        val args = ito.actualParameterTypes.map(p => Type(p.infoClass)).toList
        inspected = RType.unwindType(reflect)( AppliedType(Type(clazz), args) )
      case None      =>
        inspected = RType.unwindType(reflect)( Type(clazz) )
    }
