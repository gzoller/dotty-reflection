package co.blocke.dotty_reflection

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{Pickler,Flatten}

import info._


/** Compiler plugin (phase) that will tap into any TypeDefs, and identify any classes being defined.
 *  It will then pre-reflect on thta class and serialized the resultant RType into an annotation.
 *  This saves 2-6 sec of "priming" time when reflecting on a class using Tasty Inspection (runtime).
 */

import java.io._
object Stuff:
  trait TravelerI extends Serializable:
    val msg: String
    def serialize: String =
      val baos = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream( baos )
      oos.writeObject( this )
      oos.close()
      java.util.Base64.getEncoder().encodeToString(baos.toByteArray())

case class TravelerG(msg: String) extends Stuff.TravelerI

class ReflectionWorker extends StandardPlugin {
  val name: String = "reflectionWorker"
  override val description: String = "heavy-lift reflection worker"

  def init(options: List[String]): List[PluginPhase] = (new ReflectionWorkerPhase) :: Nil
}

class ReflectionWorkerPhase extends PluginPhase {
  import tpd._

  val phaseName = "reflectionWorker"

  override val runsAfter = Set(Pickler.name)
  // override val runsBefore = Set("genSJSIR")  // didn't change outcome!

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = 
    if tree.isClassDef && !tree.rhs.symbol.isStatic then  // only look at classes & traits, not objects
      // Reflect on the type (generate an RType), then serialize to string and add the S3Reflection annotation to the class.
      val reflect = dotty.tools.dotc.tastyreflect.ReflectionImpl(ctx)
      val reflected = RType.unwindType(reflect)(tree.tpe.asInstanceOf[reflect.Type])
      // println("REFLECTED: "+reflected)
      val s3ReflectionClassSymbol = getClassIfDefined("co.blocke.dotty_reflection.S3Reflection")
      val annoArg = NamedArg("rtype".toTermName, Literal(Constant( reflected.serialize )))
      tree.symbol.addAnnotation(Annotation.apply(s3ReflectionClassSymbol.asInstanceOf[ClassSymbol], annoArg) )
    tree

}
