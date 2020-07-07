package co.blocke.dotty_reflection

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Names._
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

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = 
    val s3ReflectionClassSymbol = ctx.getClassIfDefined("co.blocke.dotty_reflection.S3Reflection".toTermName)
    if tree.isClassDef then
      println("ClassDef found: "+tree.name.toTypeName)
      // println("   Symbol: "+tree.symbol)
      // val anno = tree.symbol.denot.annotations.find(_.symbol.fullName.toString == "com.boom.Perk")
      // println("   Anno: "+anno)
      // if anno.isDefined then
      //   val oneArg = anno.get.arguments.head
      //   val simple: SimpleName = oneArg.asInstanceOf[NamedArg].name.asInstanceOf[SimpleName]
      //   println("       Anno arg: "+anno.get.arguments + " >>> "+simple)
      //   println(s"       Simple:  start(${simple.start})  len(${simple.length})")
      // println("   Fullname: "+tree.symbol.fullName)


      // println("::: Tree Class  : "+tree.getClass.getName)
      // println("::: Symbol Class: "+tree.symbol.getClass.getName)
      // println("::: Tree rhs    : "+tree.rhs)
      // ::: Tree Class  : dotty.tools.dotc.ast.Trees$TypeDef
      // ::: Symbol Class: dotty.tools.dotc.core.Symbols$ClassSymbol

      val annoArg = NamedArg("rtype".toTermName, Literal(Constant("hey-o!")))
      tree.symbol.addAnnotation(Annotation.apply(s3ReflectionClassSymbol.asInstanceOf[ClassSymbol], annoArg) )
      tree
      // val newAnnos = tree.symbol.denot.annotations :+ Annotation.apply(s3ReflectionClassSymbol.asInstanceOf[ClassSymbol], annoArg) 

      // println("  >>> New annos: "+newAnnos)
      // val modTree = tree.withAnnotations(newAnnos.map(_.tree)) //ThisTree[Untyped]
      // This( modTree.symbol.asInstanceOf[ClassSymbol] )

      // tree.subst( List(tree.symbol), List(modTree.symbol) )

    else
      tree
      // val isAnno = tree.denot
        /*tree.tpe match {
        case a: AnnotatedType => "Yes!"
        case a => "Nope. "+a
      }
      */
      // println("   Annotated? "+ isAnno)

}
//ValDef Found!: ValDef(Result,Ident(Result$),Apply(Select(New(Ident(Result$)),<init>),List()))

//
//  Project #1: See what kind of Tree an Annotation has, how it relates to the Class it decorates.
//


// class ReflectionWorkerPhase extends PluginPhase {
//   import tpd._

//   val phaseName = "myFunPhase"

//   override val runsAfter = Set(Pickler.name)

//   override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = 
//     val myAnnotationClassSymbol = ctx.getClassIfDefined("co.blocke.poc.MyAnnotation".toTermName)
//     if tree.isClassDef then
//       val annoArg = NamedArg("id".toTermName, Literal(Constant("hey-o!")))  // @MyAnnotation(id: String)
//       tree.symbol.addAnnotation(Annotation.apply(myAnnotationClassSymbol.asInstanceOf[ClassSymbol], annoArg))
//     //   val newAnnos = tree.symbol.denot.annotations :+ Annotation.apply(myAnnotationClassSymbol.asInstanceOf[ClassSymbol], annoArg) 
//     //   val modTree = tree.withAnnotations(newAnnos.map(_.tree))  // returns ThisTree[Untyped]
//     //   tree.subst( List(tree.symbol), List(modTree.symbol) )
//     // else
//     //   tree
//     tree
// }