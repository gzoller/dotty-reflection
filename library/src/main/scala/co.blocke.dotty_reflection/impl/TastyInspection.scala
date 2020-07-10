package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tools.dotc.ast.Trees.AppliedTypeTree
  
class TastyInspection[T](clazz: Class[_], inTermsOf: TraitInfo) extends TastyInspector:

  var inspected: RType = UnknownInfo(clazz.getName)

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}
    val args = inTermsOf.actualParameterTypes.map(p => Type(p.infoClass)).toList
    if args.isEmpty then
      inspected = RType.unwindType(reflect)( Type(clazz) )
    else
      inspected = RType.unwindType(reflect)( AppliedType(Type(clazz), args) )