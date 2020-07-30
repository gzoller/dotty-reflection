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

    val pre = RType.unwindType(reflect)( Type(clazz), inTermsOf.isEmpty )
    // println("PRE: "+pre.show())
    // println("======")
    // println("ITO: "+inTermsOf.get.show())

    inTermsOf match {
      case Some(ito) =>
        /* Ok, so InTermsOf a trait is a complex process.  Going in we have fully known types for the trait.
         * 1) We do a pre-scan of the class to find out where the type symbols are.
         * 2) We think of the pre-scanned class in terms of (same fields) as the given trait (re-framed scan)
         * 3) From this re-framed scan we traverse it to find the paths to each needed type symbol
         * 4) We apply the paths to the known trait to get the RType of each type symbol
         * 5) Convert the RType to a reflect.Type so we can build the final AppliedType for clazz with the
         *     proper/known types.
         */
        val clazzSyms = clazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
        val (symPaths, unfoundSyms) = pre.findPaths(clazzSyms.map( sym => (sym->Path(Nil)) ).toMap, Some(ito))

        val args: List[reflect.Type] = clazzSyms.map( _ match {
          case sym if unfoundSyms.contains(sym) => Type(classOf[Any])
          case sym => symPaths(sym).nav(ito).getOrElse( throw new ReflectException(s"Failure to resolve type parameter '${sym}'")).toType(reflect)
          })

        inspected = RType.unwindType(reflect)( AppliedType(Type(clazz), args) )

      // Easy case... no inTermsOf... just reflect on the class.
      case None      =>
        inspected = pre
    }
