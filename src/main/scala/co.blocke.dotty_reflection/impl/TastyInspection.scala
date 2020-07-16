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

    val pre = RType.unwindType(reflect)( Type(clazz) )

    inTermsOf match {
      case Some(ito) =>
        println("HEY: "+ito)
        println("-=-=-=-=-=-=-")
        println("PRE: "+pre)

        // We have to pre-reflect the class to get the position of all the type parameters.
        // These are then mapped against the given inTermsOf TraitInfo via a fancy visitor.
        // Finally once we know the actual "exploded" actual types, we can construct the args
        // list and build the correct AppliedType.


        // Hard-wiring this works!  Now we need to computationally resolve these types from reflected Tree.
        val args = List(Type(classOf[String]),Type(classOf[Boolean]),Type(classOf[Int]))

        // val args = ito.actualParameterTypes.map{ p =>
        //    val t = p.toType(reflect).asInstanceOf[reflect.Type]
        //    println("   arg: "+t.show)
        //    t
        // }.toList

        // PROBLEM:: Resolved is wrong!
        // It produced: co.blocke.dotty_reflection.BaseClass[co.blocke.dotty_reflection.Level1[java.lang.String, scala.Boolean], scala.Int]
        // Should be  : co.blocke.dotty_reflection.BaseClass[java.lang.String, scala.Boolean, scala.Int]
        println("===============< Resolved")
        println(AppliedType(Type(clazz), args).show)
        println("=========================\n")
        // .... This explodes ....
        println(RType.unwindType(reflect)( AppliedType(Type(clazz), args) ))
        println("=========================\n")
        UnknownInfo("oops")
        // inspected = RType.unwindType(reflect)( AppliedType(Type(clazz), args) )

      // Easy case... no inTermsOf... just reflect on the class.
      case None      =>
        inspected = pre
    }


    /*
trait T5[X, Y] { val thing1: X; val thing2: Y }
trait T10[X, Y] { val x: X; val y: Y }
trait T11[W, Z] { val w: W; val z: Z }
case class TBlah1[A, B](w: A, z: B) extends T11[A, B]
case class TBar7[A, B](thing1: A, thing2: B) extends T5[A, B]
case class TFoo6[A, B, C, D](x: T11[C, T5[D, A]], y: B) extends T10[T11[C, T5[D, A]], B]


   arg: AppliedType(
     TypeRef(ThisType(TypeRef(NoPrefix,module class dotty_reflection)),trait T11),
     List(
       TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int), 
       AppliedType(
         TypeRef(ThisType(TypeRef(NoPrefix,module class dotty_reflection)),trait T5),
         List(
           TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Double), 
           TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Char)
         )
       )
     )
   )

   arg: TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class String)
    */