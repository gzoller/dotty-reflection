package co.blocke.dotty_reflection

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{Pickler, Staging}

class ReflectionWorker extends StandardPlugin {
  val name: String = "reflectionWorker"
  override val description: String = "heavy-lift reflection worker"

  def init(options: List[String]): List[PluginPhase] = (new ReflectionWorkerPhase) :: Nil
}

class ReflectionWorkerPhase extends PluginPhase {
  import tpd._

  val phaseName = "reflectionWorker"

  override val runsAfter = Set(Pickler.name)
  // override val runsBefore = Set(Staging.name)

  override def transformInlined(tree: Inlined)(implicit ctx: Context): Tree = 
    println("Inline Found!: " )
    tree.expansion
}
//ValDef Found!: ValDef(Result,Ident(Result$),Apply(Select(New(Ident(Result$)),<init>),List()))