package co.blocke.dotty_reflection

import model._
import impl.Clazzes._
import scala.util.Try

trait TypeShell[X] { val x: X }
case class TypeShellHolder(a: TypeShell[Int])


@main def runme(): Unit =

  // try {
    val z = Reflector.reflectOn[TypeShellHolder]
    println(z)
    // println(z.asInstanceOf[ScalaClassInfo].constructWith[Blah](List(Foom(5,Left("Greg"),1.2))) )
    
  // } catch {
  //   case x => //x.printStackTrace()
  // }
