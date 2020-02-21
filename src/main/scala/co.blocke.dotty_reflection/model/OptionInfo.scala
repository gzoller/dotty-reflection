package co.blocke.dotty_reflection
package model

import java.lang.reflect._
import java.util.Optional

trait OptionInfo extends ReflectedThing {
  val optionParamType: ALL_TYPE

  def isA(c: Class[_]): Boolean = false // should never be called
}

case class ScalaOptionInfo(
  name: String,
  optionParamType: ALL_TYPE
) extends OptionInfo {
  val typeParameters = Nil

  def isA2( arg: Object ): Boolean =
    optionParamType match {
      case _:TypeSymbol => throw new Exception("Unexpected type symbol")
      case ct:ReflectedThing =>
        arg == None || (arg.isInstanceOf[Some[_]] && ct.isA(arg.asInstanceOf[Some[_]].get.getClass))
      case ct:PrimitiveType =>
        arg == None || (arg.isInstanceOf[Some[_]] && ct.isA(arg.asInstanceOf[Some[_]].get.getClass))
      }
}

case class JavaOptionInfo(
  name: String,
  optionParamType: ALL_TYPE
) extends OptionInfo {
  val typeParameters = Nil

  def isA2( arg: Object ): Boolean =
    arg.isInstanceOf[Optional[_]] && {
      val optionalArg = arg.asInstanceOf[Optional[_]]
      optionParamType match {
        case _:TypeSymbol => throw new Exception("Unexpected type symbol")
        case ct:ReflectedThing =>
          optionalArg.isEmpty || ct.isA(optionalArg.get.getClass)
        case ct:PrimitiveType =>
          optionalArg.isEmpty || ct.isA(optionalArg.get.getClass)
        }
    }
}