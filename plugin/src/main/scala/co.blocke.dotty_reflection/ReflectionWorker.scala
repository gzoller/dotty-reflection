package co.blocke.dotty_reflection

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.Pickler

/** Compiler plugin (phase) that will tap into any TypeDefs, and identify any classes being defined.
 *  It will then pre-reflect on thta class and serialized the resultant RType into an annotation.
 *  This saves 2-6 sec of "priming" time when reflecting on a class using Tasty Inspection (runtime).
 */

class ReflectionWorker extends StandardPlugin {
  val name: String = "reflectionWorker"
  override val description: String = "heavy-lift reflection worker"

  def init(options: List[String]): List[PluginPhase] = (new ReflectionWorkerPhase) :: Nil
}

class ReflectionWorkerPhase extends PluginPhase {
  import tpd._

  val phaseName = "reflectionWorker"

  override val runsAfter = Set(Pickler.name)

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = 
    val s3ReflectionClassSymbol = ctx.getClassIfDefined("co.blocke.dotty_reflection.S3Reflection".toTermName)
    if tree.isClassDef then
      val annoArg = NamedArg("rtype".toTermName, Literal(Constant("hey-o!")))
      tree.symbol.addAnnotation(Annotation.apply(s3ReflectionClassSymbol.asInstanceOf[ClassSymbol], annoArg) )
    tree

}
