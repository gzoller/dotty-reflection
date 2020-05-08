package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector


/**
 * For Reflector.reflectOnClassInTermsOf() we need to pre-inspect a class to determine parentage.  We only need to reflect on the class' 
 * parentage tho, so we can save time by ignoring all the deeper reflection into fields, etc.  It returns a partially-populated 
 * ScalaCaseClassInfo object--just enough for reflectOnClassInTermsOf().
 */
class ScalaClassInspectorLite(clazz: Class[_]) extends TastyInspector with ScalaClassInspectorLike with ParamGraph:
  import Clazzes._

  var inspected: RType = UnknownInfo(clazz)

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}
    inspected = inspectClass(clazz.getName, reflect)(root)
    

  def inspectClass(className: String, reflect: Reflection)(tree: reflect.Tree): RType =
    import reflect.{given _}

    object Descended {
      def unapply(t: reflect.Tree): Option[RType] = descendInto(className, reflect)(t)
    }
  
    // We expect a certain structure:  PackageClause, which contains ClassDef's for target class + companion object
    val foundClass = tree match {
      case t: reflect.PackageClause => 
        t.stats collectFirst {
          case Descended(m) => m
        }
      case t => 
        None  // not what we expected!
    }
    foundClass.getOrElse( UnknownInfo(Class.forName(className)) )


  private def descendInto(className: String, reflect: Reflection)(tree: reflect.Tree): Option[RType] =
    import reflect.{_, given _}
    tree match {

      case pkg: reflect.PackageClause => // nested packages
        Some(inspectClass(className, reflect)(tree))

      case t: reflect.ClassDef if !t.name.endsWith("$") =>

        // Get any type parameters
        val typeParams = clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList

        val inspected: RType =
          // === Trait ===
          if(t.symbol.flags.is(reflect.Flags.Trait)) then
            val actualTypeParams = typeParams.map(p => TypeSymbolInfo(p.asInstanceOf[String]))
            val traitInfo = TraitInfo(className, clazz, typeParams, actualTypeParams)
            registerParents(reflect)(t, traitInfo) // Now figure out type parameter graph
            traitInfo

          else
            // === Scala Class (case or non-case) ===
            val classInfo = ScalaCaseClassInfo(className, clazz, typeParams, Nil, Nil, null, false)

            // Now figure out type parameter graph
            registerParents(reflect)(t, classInfo)

            classInfo

        
        Some(inspected)

      case _ =>
        None
    }