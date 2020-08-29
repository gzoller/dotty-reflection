package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tools.dotc.ast.Trees.AppliedTypeTree
  
class TastyInspection(clazz: Class[_]) extends TastyInspector:

  var inspected: Transporter.RType = UnknownInfo(clazz.getName)

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}
    RType.unwindType(reflect)( Type(clazz), false )
